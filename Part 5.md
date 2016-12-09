# Part 5
[Part 0](Part 0.md) [Part 1](Part 1.md) [Part 2](Part 2.md) [Part 3](Part 3.md) [Part 4](Part 4.md)

## Resources & Documentation
Here are links to relevant documentation and resource pages that will be useful to refer to for this tutorial.

* [Radiance](https://shirakumo.github.io/radiance)
* [Interface Definitions](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp)
* [Clip](https://shinmera.github.io/clip)
* [lQuery](https://shinmera.github.io/lquery)
* [LASS](https://shinmera.github.io/LASS)
* [Crypto-Shortcuts](https://shinmera.github.io/crypto-shortcuts)

## A Short Roadmap
Our ToDo list for this part looks as follows:

* Tying pastes to user accounts
* Rudimentary spam protection
* Showing pastes on a user's profile

Lets try to tick it all off.

## Paste Authorship
An often required part of web applications is some kind of user account to identify different people and tie data to them. Since one of Radiance's primary goals is to allow the co-existence of multiple applications in the same instance that all share common resources, providing users is of course an important aspect.

Users are, just like the database, provided through an interface. Let's add it to the system.

```common-lisp
(asdf:defsystem plaster
  ...
  :depends-on ((:interface :user)
               ...))
```

Note that the user system by itself does not handle authentication, profiles, or anything beyond the absolute basics of user objects and permissions. Permissions are so tightly tied to the users, and have to be standardised across all applications, that it makes sense to couple them together. Before we jump into permissions though, let's just take care of registering the author with a paste.

For this, we'll need another field in our database schema. Users don't have a unique ID, but rather a unique username string that can be up to 32 characters long. Thus, our schema needs a field like the following:

```common-lisp
(author (:varchar 32))
```

Naturally our creation function needs a new argument to account for this, too.

```common-lisp
(defun create-paste (text &key title parent visibility password author)
  (when (and parent visibility)
    (api-error "Cannot set the visibility of an annotation."))
  (db:with-transaction ()
    (let* ((paste (dm:hull 'plaster-pastes))
           (parent (when parent (ensure-paste parent)))
           (visibility (if parent 2 (ensure-visibility visibility)))
           (password (ensure-password visibility password)))
      (setf (dm:field paste "text") text
            (dm:field paste "title") (or title "")
            (dm:field paste "time") (get-universal-time)
            (dm:field paste "author") author
            (dm:field paste "visibility") visibility
            (dm:field paste "password") password)
      (dm:insert paste)
      (when parent (register-annotation paste parent))
      paste)))
```



## Spam Protection

## A User Profile

[Part 6](Part 6.md)
