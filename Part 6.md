# Part 6
[Part 0](Part 0.md) [Part 1](Part 1.md) [Part 2](Part 2.md) [Part 3](Part 3.md) [Part 4](Part 4.md) [Part 5](Part 5.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)
* [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts)

## A Short Roadmap
This is the last part that deals with actual development of our application. The final part is one from the perspective of an administrator.

* A capable editor with syntax highlighting
* Missing API endpoints
* Configuration and user settings

Let's wrap up our development, then.

## Syntax Highlighting
What we've been missing all this time is syntax highlighting. I've avoided this topic, because it's something that is much more on the side of the client than on the side of the server. Indeed, we're going to use an already available JavaScript solution to do the markup and editing support for us for the most part. We're going to employ the help of [CodeMirror](http://codemirror.net/), which is an excellent editor solution.

However, in order to properly be able to mark up the text, we need a new field on our pastes to track what kind of data it is supposed to be. 

STUFFFFFFF

## Missing API Endpoints
We've created API endpoints to handle the necessary data manipulation, but we don't have API equivalents of our view, list, and user pages yet. The logic won't be much to look at, but thinking about sending back paste data does raise one issue that I've forgotten about.

Namely, currently we're outputting all the fields from a paste. This includes the password field. Even if it is hashed, and thus relatively secure, we still definitely won't want to output that. First step, then, is to create a formatter function that translates a paste data-model instance into something we can simply spit out of the API.

```common-lisp
(defun reformat-paste (paste &key include-annotations)
  (let ((table (make-hash-table :test 'eql)))
    (flet ((copy (field)
             (setf (gethash field table) (dm:field paste field))))
      (mapcar #'copy '("title" "time" "author" "visibility" "text" "type")))
    (when include-annotations
      (setf (gethash "annotations" table)
            (mapcar #'reformat-paste (paste-annotations paste))))
    table))

(defun api-paste-output (paste)
  (cond ((string= "true" (post/get "browser"))
         (redirect (paste-url paste)))
        (T
         (api-output (reformat-paste paste)))))
```

I've also included the ability to add the list of annotations into the output, which I'm sure would be useful to have as well. Moving on!

```common-lisp
(define-api plaster/view (id &optional current-password include-annotations) ()
  (let ((paste (ensure-paste id)))
    (check-permission 'view paste)
    (with-password-protection (paste current-password)
      (api-output (reformat-paste paste :include-annotations (or* include-annotations))))))
```

Viewing a single paste is easy enough. Listing a bunch of paste is a bit more difficult, since we need to account for multiple clauses like filtering by author and handling pagination. We should probably also limit the number of pastes that can be returned at max, just to rate limit things a bit.

```common-lisp
(defparameter *default-api-amount* 50)
(defparameter *maximum-api-amount* 100)

(define-api plaster/list (&optional author skip amount include-annotations) ()
  (check-permission 'list)
  (let ((amount (if amount (parse-integer amount) *default-api-amount*))
        (skip (if skip (parse-integer skip) 0)))
    (unless (<= 0 amount *maximum-api-amount*)
      (error 'api-argument-invalid :argument "amount"
                                   :message (format NIL "Amount must be within [0,~a]" amount)))
    (let ((query (cond ((and (auth:current) (equalp author (user:username (auth:current))))
                        (db:query (:= 'author author)))
                       (author
                        (db:query (:and (:= 'visibility 1)
                                        (:= 'author author))))
                       (T
                        (db:query (:= 'visibility 1))))))
      (api-output
       (loop for paste in (dm:get 'plaster-pastes query
                                  :sort '((time :DESC))
                                  :amount amount
                                  :skip skip)
             collect (reformat-paste paste :include-annotations include-annotations))))))
```

This looks a bit daunting at first, indeed. It starts with your standard argument validation shpiel; we check for permissions, check that the arguments are parsable and within the right range, and then construct a fitting query. Sadly the `db:query` construct does not allow you to use conditionals on the "client side", so we need to construct three separate queries depending on whether we're viewing ourselves, someone else, or the public listing. Finally we fetch the result list and push it through our `reformat-paste` function to get the proper format.

Naturally you could add a bunch more endpoints and arguments to allow more flexible querying of which exact results you would like to see. For now, I will consider this sufficient and leave the extension of this aspect of the application up to you.

## User Settings


## Configuration


[Part 7](Part 7.md)
