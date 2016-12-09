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
* Rudimentary protection
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

Naturally we need to pass the author to it from our API endpoint. But hold on, where do we even know which user it is from? The [user interface](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp#L63) does not give us anything that can do that for us. Indeed, we won't be able to get what we need with that interface alone. We need something else as well, namely authentication. For this, too, we have an interface, conveniently called [`auth`](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp#L37). I trust you are able to add it to the system dependencies on your own.

Using `auth:current`, we can then set the proper author on our `plaster/new` endpoint.

```common-lisp
:author (user:username (or (auth:current) (user:get "anonymous")))
```

`auth:current` is specified to return the user object that is tied to the current request, if any. How exactly this tie works is up to the implementation, but usually involves another interface for sessions, which I won't get into now. If there is no object tied to the request, NIL is returned, which is why we `or` it with an explicit user fetch. The user interface guarantees that there's always a user object with the name `"anonymous"`. This is useful because it will always allow you to regulate the permissions of unauthorised users.

With all set on the storage side already, all we need to do to close the deal on this is to actually display the information on the templates. Adding the following to the header of the edit form and to the link of each entry in the list should suffice.

```HTML
<a rel="author" lquery="(text author)">AUTHOR</a>
```

And we're done.

## Rudimentary Protection
Now that we have users and tracking thereof, we should revise our protections. For example, we probably want a permission to control whether someone can paste at all. We should also restrict editing and deleting of a paste to the author himself and perhaps administrators.

First I should explain how permissions work in Radiance. Each user has a list of granted permissions, each of which is in itself a list of at least one element. Permissions can also be represented as a string with dots separating each element. Let's look at some examples.

```
plaster
plaster paste edit own
plaster paste edit
```

The first permission, if granted, would give you access to all of the permissions that start with the `plaster` element. The second one would allow you access to, supposedly, editing your own pastes. The third one however would allow you to edit all pastes. More specifically, each element in the permission makes it more specific, but a permission always grants everything below its lowest element.

Let's look at the concrete permissions we'll want to use for Plaster:

```
plaster paste new
plaster paste list
plaster paste view
plaster paste view own
plaster paste edit
plaster paste edit own
plaster paste delete
plaster paste delete own
```

Where applicable I've created both a permission for the action, and for the special case when the action is performed on your own creation. The granted default permissions for the anonymous user for example will be:

```
plaster paste new
plaster paste view
plaster paste list
```

Whereas a standard user should have the following ones:

```
plaster paste new
plaster paste view
plaster paste list
plaster paste edit own
plaster paste delete own
```

And an admin should just have everything.

```
plaster
```

Ok, so let's record this information.

```common-lisp
(user:add-default-permissions
 (perm plaser paste new)
 (perm plaser paste view)
 (perm plaser paste list)
 (perm plaser paste edit own)
 (perm plaser paste delete own))

(user:add-default-anonymous-permissions
 (perm plaster paste new)
 (perm plaster paste view)
 (perm plaster paste list))
```

We can just add this as a toplevel form. Any new user that is created from here on out will automatically receive these permissions. The `perm` form here is a special macro that will record the permission in your module, so that it can be easily inspected. Just try it! `(describe (radiance:module :plaster))`

Now we need to actually add checks into the pages and API endpoints. Once again, we'll make a short helper function to shorten things.

```common-lisp
(defun check-permission (paste action &optional (user (or (auth:current) (user:get "anonymous"))))
  (unless (or (and paste
                   (string= (dm:field paste "author") (user:username user))
                   (user:check user `(plaster paste ,action own)))
              (user:check user `(plaster paste ,action)))
    (error 'request-denied :message (format NIL "You do not have the permission to ~a pastes."
                                            action))))
```

We use the regular structure of our permissions to do most of the work. If a paste is given, we pass the check if the author is the same and we are allowed to edit our own pastes. Otherwise we have to check the general permission. 

Now all we need to do is sprinkle calls to the function all over. For example, the edit page should have this:

```common-lisp
(if id
    (check-permission NIL 'new)
    (check-permission paste 'edit))
```

And the edit endpoint should have this:

```common-lisp
(check-permission paste 'edit)
```

I will let you figure out what to do with the other API endpoints and pages yourself. To close the deal on the permissions, we need one final tweak, which is to adapt the buttons on the templates to only show up if the user can even perform the related action.

## A User Profile

[Part 6](Part 6.md)
