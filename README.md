simple-search: An in-memory full-text search system for CL
==========================================================

simple-search allows you to index documents you have stored *in-memory* and
query them in various ways. It's goals are compactness, simplicity, and ease of
use. Performance is somewhat important, but secondary.

It exists as a smaller alternative to Montezuma where you don't mind re-creating
your indexes every time you load your app.

Feature list:

- Full-text indexing with optional stemming support
- Ability to unindex or re-index documents
- Phrase searches (which [require manual work](#query-function))
- Tree-based query language using three primitive operators: `and`, `or`, `not`
- Sorting on fields in documents
- Offset/limiting of result sets

List of features not supported (although they may be supported in the future if
people bug me enough):

- Built-in, natural phrase searching
- Sorting based on relevance: currently a document matches or it doesn't
- Persisting to disk
- Range queries

Note that I'm not a search expert and have probably done some stupid things in
this library. Feel free to open an issue or PR (or just email me) if you notice
something awry.

## Documentation

- [document](#document-class)
- [make-document](#make-document-function)
- [index (class)](#index-class)
  - [documents](#documents-accessor)
- [make-index](#make-index-function)
- [index](#index-function)
- [unindex](#unindex-function)
- [phrase-search](#phrase-search-function)
- [query](#query)

### document (class)
This is an opaque class created by [make-document](#make-document-function). It
houses information about a document you wish to index.

### make-document (function)
```lisp
(defun make-document (field-descriptions data &key reference))
  => document
```
Create a document out of a set of field descriptions (essentially a schema) and
a *hash table* of data. Also, you can optionally specify a reference value which
will be passed into any `phrase-fn` (see `[query](#query-function)`) and can be
used to pre-calculate phrase strings for a document:

```lisp
(let ((doc-data (let ((hash (make-hash-table :test 'equal)))
                  ;; yes, I know setf can take multiple pairs but I think that's ugly and I refuse to do it.
                  (setf (gethash "id" hash) "1234")
                  (setf (gethash "name" hash) "barthalomew")
                  ;; note that we can pass a list of words here and it works fine (nice for tagged data)
                  (setf (gethash "tags" hash) '("unhappy" "rigid" "friendless"))
                  (setf (gethash "num-friends" hash) 0)
                  hash)))
  (make-document
    '(("id")
      ("name" :tokenize t)
      ("tags" :tokenize t)
      ("num-friends" :sort t))
    doc-data
    ;; here we optionally create a string we can use later to match phrases against
    :reference (concatenate 'string (gethash "name" doc-data)
                                    (reduce (lambda (a b)
                                              (concatenate 'string a " " b))
                                            (gethash "tags" doc-data)
                                            :initial-value ""))))
```

The schema must be passed each time, so best to wrap the creation of commong
documents n your own helper functions (ie `make-dog` or `make-grandparent`) that
just take a hash table as the only arg.

Note that the `id` field is *mandatory* and it can't be called something else.
Also, `id` always creates a sort entry, so no need to specify `:sort` for it.

`:tokenize` in the schema tells the indexer whether to split that field into
separate words (good for indexing, bad for things like IDs).

`:sort` tells the indexer to store the value of the field in the index so it can
be sorted on when querying. Sorting is special because other that tokenizing the
words and creating word => doc-id lookups, all other information about the
document is thrown away.

### index (class)
The index class holds a full-text index. It is created using [make-index](#make-index-function)
and populated via the [index](#index-function) and [unindex](#unindex-function)
functions.

It has one accessor:

##### documents (accessor)
Returns a list of document IDs indexed by this index.

### make-index (function)
```lisp
(defun make-index (&key stemming))
  => index
```
Create a new index. If `:stemming` is true, enable stemming for this index. What
this does is make words like "running" and "run" match each other where
otherwise you'd need an exact match.

### index (function)
```lisp
(defun index (index doc))
  => nil
```
Index a document into the given index. The document must have been created via
[make-document](#make-document-function), and the index via [make-index](#make-index-function).
Example:

```lisp
(let ((document (make-document '(("id") ("body" :tokenize t)) my-doc-data))
      (my-index (make-index :stemming t)))
  (index my-index document))
```

### unindex (function)
```lisp
(defun unindex (index doc-id))
  => nil
```
Unindex a document from an index. This wipes out all word => doc ID entries and
also any sort-field storage. This can be used to update a document's index:

```lisp
(let ((my-index (make-index :stemming t)))
  ;; index the document
  (index my-index (make-document '(("id") ("body" :tokenize t)) my-doc-data))
  ;; data changed
  (setf (gethash "body" my-doc-data) "i am jack's complete lack of creativity")
  ;; unindex and reindex the document
  (unindex my-index (gethash "id" my-doc-data))
  (index my-index (make-document '(("id") ("body" :tokenize t)) my-doc-data)))
```

### phrase-search (function)
```liap
(defun phrase-search (phrase body &key case-sensitive))
  => t/nil
```
Convenience function provided to facilitate phrase searches in a somewhat
barbaric manner. This function essentially wraps CL's `search` function, but may
do more later on.

To get a feel for why this exists, see the [query](#query-function) function.

### query (function)
```lisp
(defun query (index query &key sort phrase-fn (offset 0) limit))
  => list-of-doc-ids
```
Search an index! `index` is your index that you've already populated via the
[index](#index-function) function.

`query` is a query form:

```lisp
'(:and "dog" "friendly"
       (:or "fast" "protective"
            (:not "loud" (:phrase . "barks a lot")))
       (:not "bity"))
```
Notice we can nest layers of logic operations here. Note that `:not` operations
work exactly like `:or` but negates the results upon calculation. Also note that
__queries must have a logic operation as the root of the tree__: `:and`, `:or`, 
or `:not`.

`:sort` specifies a sort field and a direction: `'("num-friends" . :asc)` or
`'("id" . :desc). You can sort on any field that you specify with `:sort t` in
your document schema.  Note that the `id` field is always sortable.

`:phrase-fn` *must* be specified if you have a `:phrase` item in your query. It
makes it possible for the search engine to do phrase matching without keeping
all your documents in memory by basically farming out the work to you! Here's
how it works:

```lisp
(defparameter *my-notes* (populate-my-notes-from-db))
(defparameter *note-index* (make-index))

(dolist (note *my-notes*)
  (index *note-index* (make-document '(("id")
                                       ("title" :tokenize t)
                                       ("body" :tokenize t)
                                       ("tags" :tokenize t))
                                     note
                                     :reference (concatenate 'string
                                                             (gethash "body" note)
                                                             (reduce (lambda (a b) (concatenate 'string a " " b))
                                                                     (gethash "tags" not)
                                                                     :initial-value "")))))

(query *note-index*
       '(:and "restaurant"
              (:not "tacos" (:phrase . "good times")))
       :phrase-fn (lambda (doc-id ref phrase)
                    ;; here, `ref` is the :reference entry we passed to make-document
                    (phrase-search phrase ref)))
```

`:offset` is how many records to skip from the beginning, and `:limit` limits
the number of returned documents:

```lisp
(query *note-index*
       '(:and "trolls")
       :sort '("id")    ; :asc by default
       :offset 30
       :limit 10)
```

## Tests
```lisp
(asdf:operate 'asdf:load-op :simple-search-test)
(simple-search-test:run-tests)
```

## License
MIT.

