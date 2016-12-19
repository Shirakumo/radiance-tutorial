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
* [CodeMirror](http://codemirror.net/doc/manual.html)
* [Ubiquitous](https://shinmera.github.io/ubiquitous)

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

## Protection with Permissions
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

Ok, so let's record this information. We need to stuff it into another trigger, because we can only perform actions on users once the user system has declared itself ready and loaded (usually when the database is ready).

```common-lisp
(define-trigger user:ready ()
  (user:add-default-permissions
   (perm plaster paste new)
   (perm plaster paste view)
   (perm plaster paste list)
   (perm plaster paste edit own)
   (perm plaster paste delete own))

  (user:grant
   "anonymous"
   (perm plaster paste new)
   (perm plaster paste view)
   (perm plaster paste list)))
```

We can just add this as a toplevel form. Any new user that is created from here on out will automatically receive these permissions. The `perm` form here is a special macro that will record the permission in your module, so that it can be easily inspected. Just try it! `(describe (radiance:module :plaster))`

Now we need to actually add checks into the pages and API endpoints. Once again, we'll make a short helper function to shorten things.

```common-lisp
(defun permitted (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted a paste user))
      (or (and paste
               (equal (dm:field paste "author") (user:username user))
               (user:check user `(plaster paste ,action own)))
          (user:check user `(plaster paste ,action)))))

(defun check-permission (action &optional paste (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted action paste user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a pastes."
                                            action))))
```

We use the regular structure of our permissions to do most of the work. If a paste is given, we pass the check if the author is the same and we are allowed to edit our own pastes. Otherwise we have to check the general permission. I've also split it into a predicate and an erroring check. The former will become useful shortly.

Now all we need to do is sprinkle calls to the function all over. Most of the places should be straightforward, with the exception of the edit page, which needs a bit more special treatment. It is also the reason why I've allowed lists of action tests in `permitted`.

```common-lisp
(if id
    (check-permission '(edit delete) paste)
    (check-permission 'new))
```

This is necessary because it might be plausible to have a setup where one can delete pastes, but not edit them, or vice-versa. For example, a janitor might have permissions to delete spam, but shouldn't be able to edit random pastes. Anyway, in other places like the edit endpoint, the check is simply something like this:

```common-lisp
(check-permission 'edit paste)
```

I will let you figure out what to do with the other API endpoints and pages yourself. To close the deal on the permissions, we need one final tweak, which is to adapt the buttons on the templates to only show up if the user can even perform the related action.

First up, `view.ctml`. All we need to do is wrap the action buttons and the header nav buttons in a `<c:when>` with a test that checks if it's permitted. Since we created a predicate function exactly for this above, we can use it. Thus, the new actions of the main paste should look like this:

```HTML
<c:when test="(plaster::permitted :new)">
  <a href="#" @href="plaster/edit?annotate={0}&password={1} _id (** :password)">Annotate</a>
  <a href="#" @href="plaster/edit/{0}?repaste&password={1} _id (** :password)">Repaste</a>
</c:when>
<c:when test="(plaster::permitted :edit *)">
  <a href="#" @href="plaster/edit/{0}?password={1} _id (** :password)">Edit</a>
</c:when>
```

Being able to call arbitrary functions from the templates makes things very handy, if perhaps a bit verbose at times. All that's left is to add a similar test to `edit.ctml`'s submission buttons. We only need to change the `Delete` and `Save` ones too, since the `Post` case is already blocked by a huge error page.

```HTML
<c:when test="(plaster::permitted :delete *)">
  <input type="submit" @formaction="/api/plaster/delete" value="Delete" />
</c:when>
<c:when test="(plaster::permitted :edit *)">
  <input type="submit" @formaction="/api/plaster/edit" value="Save" />
</c:when>
```

Don't forget to adjust the nav in the head on the edit and list templates, too!

## Rate Limiting
We should probably also take care not to allow a spammer to hammer out as many pastes as they want within no time. We could trivially implement rate limiting ourselves by just capturing the IP and an access timestamp, but fortunately enough this is also a common enough problem that Radiance provides an interface for it that we can just use directly. The interface is called [rate](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp#L17).

Given that it is a very simple task, it is only justified that using the rate interface is very simple too. All we need to do is wrap the `plaster/new` API endpoint in `rate:with-limitation` and define a rate limitation behaviour using `rate:define-limit`.

```common-lisp
(rate:define-limit create (time-left :limit 2)
  (error 'api-error :message (format NIL "Please wait ~a second~:p before pasting again." time-left)))
```

The body of the definition sets the code that is executed instead of the 'real body' in the case of excessive access. You can also define the timeout and the number of calls that can be made before the timeout goes into effect. The API endpoint definition should look something like this, now:

```common-lisp
(define-api plaster/new (text &optional title type parent visibility password current-password) ()
  (rate:with-limitation (create)
    ...))
```

You could also rate limit the other endpoints and pages, but for the most part, spamming will happen on the creation endpoint, which is what we want to protect most of all.

Don't forget to add the interface to your system dependencies!

## A User Profile
Providing user profiles is yet another common problem, for which Radiance offers yet another interface, namely [`profile`](https://github.com/Shirakumo/radiance/blob/master/standard-interfaces.lisp#L92). This interface is responsible for expanding the users a bit for usage in any kind of software that wants to allow users that aren't merely internal or privately associated. As such it gives you access to arbitrary user fields, an avatar, and panels. The panels are arbitrary templates that you can render onto the profile page of a user.

Given this, there are two ways for us to offer a profile page for users on our service. We can either define a profile panel that lists all the pastes, or define our own page entirely. Panels lend themselves well for information that is directly tied to the user like posting stats and other information. Actual post objects like our pastes here should probably be on a separate page however.

We'll be doing both-- namely, we'll create a new page that lists a user's pastes, and we'll add a small panel to the user profile that shows some paste stats. The actual user paste page will be very similar to the list page we've made before. So, let's create a `user.ctml`.

```HTML
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta charset="utf-8" />
    <title>Plaster</title>
    <link rel="stylesheet" type="text/css" href="../static/plaster.css" @href="/static/plaster/plaster.css" />
  </head>
  <body>
    <header>
      <h1>Plaster</h1>
      <nav>
        <c:when test="(plaster::permitted :new)">
          <a href="#" @href="plaster/edit">New</a>
        </c:when>
        <c:when test="(plaster::permitted :list)">
          <a href="#" @href="plaster/list">List</a>
        </c:when>
      </nav>
    </header>
    <main>
      <section class="paste-list user">
        <header>
          <img alt="Avatar" src="#" lquery="(attr :src (profile:avatar user 100))" />
          <h2><a href="#" @href="<profile page {0}> user" lquery="(text username)">USERNAME</a>'s Pastes</h2>
        </header>
        <c:when test="(< 0 page)">
          <a class="button newer" href="#" @href="plaster/user/{0}/{1} username (1- page)">Newer</a>
        </c:when>
        <ul iterate="pastes">
          <li>
            <a href="#" @href="plaster/view/{0} _id">
              <span class="id" lquery="(text _id)">ID</span>
              <span class="title" lquery="(text title)">TITLE</span>
              <time lquery="(time time)" />
            </a>
          </li>
        </ul>
        <c:when test="has-more">
          <a class="button older" href="#" @href="plaster/user/{0}/{1} username (1+ page)">Older</a>
        </c:when>
      </section>
    </main>
  </body>
</html>
```

There are some interesting points to mention here. First, we employ the `profile:avatar` function and change the `src` attribute to that using the lQuery `attr` function. The avatar function requires you to pass a "suitable size" for the avatar and will try (but not guarantee!) to return something close to that size. While you can always resize the image with CSS, you shouldn't set it unreasonably large as that might significantly slow down page speed.

Second, in order to link to the user's profile we're using a different kind of pattern: `<profile page {0}>`. A pattern like that is called a resource and is resolved through Radiance's resource system. The resource system allows you to access certain relevant information of a module or interface through a standardised mechanism. Resources are dispatched by the module you're requesting information from, and the type of resource you need. The page resource type we're employing here is specified to always return a URI to the requested page. For the profile interface, this means a URI to the page that displays the given user's profile. You can try it out at the REPL too, using the `resource` function.

Everything else that's in the template should be familiar already. Time to create the corresponding page.

```common-lisp
(define-page user "plaster/user/(.*)(?:/(.*))?" (:uri-groups (username page) :clip "user.ctml")
  (check-permission 'user)
  (let* ((page (or (when page (parse-integer page :junk-allowed T)) 0))
         (user (user:get username)))
    (unless user
      (error 'request-not-found :message (format NIL "No such user ~s." username)))
    (let ((pastes (dm:get 'plaster-pastes
                          (if (and (auth:current) (or (eql (auth:current) user)
                                                      (user:check (auth:current) '(perm plaster))))
                              (db:query (:= 'author username))
                              (db:query (:and (:= 'author username)
                                              (:= 'visibility 1))))
                          :sort '((time :DESC))
                          :skip (* page *pastes-per-page*)
                          :amount *pastes-per-page*)))
      (r-clip:process T :pastes pastes
                        :user user
                        :username (user:username user)
                        :page page
                        :has-more (<= *pastes-per-page* (length pastes))))))
```

The primary difference to the `list` page is that I'm fetching the user object according to the URL argument, error out if it can't be found, and then display a different number of pastes depending on whether the user views their own page or are an administrator. After all, it is only sensible that you should be able to view all of your own pastes, not just the public ones. Don't forget to add the permission to the `user:ready` trigger.

You should be able to view [anonymous' profile](http://localhost:8080/!/plaster/user/anonymous) now. But, we're not quite done yet. For a first step we'll want to turn the `author` links we have in the `view` and `list` pages into actually useful ones. Adding the matching `@href` attribute should already do the trick.

```HTML
@href="plaster/user/{0} author"
```

Finally we should have a button that lets you visit your own profile, or redirects you to the login page should you not be logged in already. We can do that, too.

```HTML
<c:if test="(auth:current)">
  <c:then><c:when test="(plaster::permitted :user)">
    <a href="#" @href="plaster/user/{0} (user:username (auth:current))">My Pastes</a>
  </c:when></c:then>
  <c:else>
    <a href="#" @href="<auth page login #>">Login</a>
  </c:else>
</c:if>
```

A bit more involved than most template logic we've seen so far, but still not immensely difficult to parse. If the user is not logged in, a link to the login page is displayed. This happens through a resource request again, just like the profile link. If the user is logged in and is allowed to view a profile's pastes, we present the link to their own page. It's as simple as that. 

You might be wondering what the `#` is for in the `auth` resource request. That argument is specially treated by the interface and tells it to redirect back to the page we are currently from once the login is complete.

Alright, I suppose it is high time that we actually played around with a user account that isn't anonymous. We're going to do some interface stuff to create a new account for ourselves, and then tie our current session to it to bypass the traditional login screen. First, creating a new user happens with the `user:get` function, we just have to tell it to create a new user if it doesn't exist yet.

```common-lisp
(user:get "pester" :if-does-not-exist :create)
```

Now that we have a user, we can tell the auth interface to associate our session with it. First, let's look at what kinds of sessions we have:

```common-lisp
(session:list)
```

This should return a list of one session object-- namely the one you've been using to access the site with your own browser. We can now use this object and tell auth that it is logged in.

```common-lisp
(auth:associate (user:get "pester") (first (session:list)))
```

Once you refresh the page in your browser now, you should see by the changed button in the header that you've been logged in. Miraculous! Using `user:grant` and `user:revoke` you can now also play around with the various permissions and check that everything works as intended.

Finally we'll want to add a user profile panel. Time for one last, final template, `user-panel.ctml`.

```HTML
<link rel="stylesheet" type="text/css" href="../static/panel.css" @href="/static/plaster/panel.css" />
<section class="paste-list panel">
  <h2>Recent Pastes</h2>
  <ul iterate="pastes">
    <li>
      <a href="#" @href="plaster/view/{0} _id">
        <span class="id" lquery="(text _id)">ID</span>
        <span class="title" lquery="(text title)">TITLE</span>
        <time lquery="(time time)" />
      </a>
    </li>
  </ul>
</section>
```

Since we need something that integrates into a larger template, we don't need all of the HTML boilerplate, just the actual content. We will however link to another, much smaller stylesheet, in order to make things not look quite as gross as they would being completely unstyled. Can you remember how we created the CSS file the first time around?

That's right, create a file `panel.lass` in the `static/` directory, fill it with some LASS content, and compile it down.

```common-lisp
(.paste-list
 (ul
  :list-style none
  :padding 0
  :margin 0
  (li (a :background none
         :display flex
         :align-items center
         :color (rgb 0 0 0)
         :font-weight normal
         :text-decoration none
         (.id :min-width 50px)
         (.title :flex-grow 1))
      ("a:hover"
       :background (rgb 220 220 220)))))
```

The last thing we'll need for our coup de grâce to be complete is the actual panel definition. The profile interface specifies a `profile:define-panel` macro that we can use for exactly this purpose. Observe and recreate.

```common-lisp
(profile:define-panel pastes (:user user :clip "user-panel.ctml")
  (let ((pastes (dm:get 'plaster-pastes
                        (if (and (auth:current) (or (eql (auth:current) user)
                                                    (user:check (auth:current) '(perm plaster))))
                            (db:query (:= 'author (user:username user)))
                            (db:query (:and (:= 'author (user:username user))
                                            (:= 'visibility 1))))
                        :sort '((time :DESC))
                        :amount *pastes-per-page*)))
    (r-clip:process T :pastes pastes)))
```

This is very, very similar in behaviour to the `define-page` macro we already know well. Interesting is the addition of the `:user` option that lets us bind the current user object to a variable. The rest is mostly just a short transcription of what happens on the `user` page. Hit `C-c C-c` and stare at the magnificence of the [profile panel](http://localhost:8080/!/user/anonymous/pastes).

Truly stunning.

## Conclusion
Once you've recovered from being awe-struck and have ascertained that you have properly understood everything in this part, you may move on to the next one. There, we'll start to slowly wrap things up by adding all the rest of the missing functionality.

[Part 6](Part 6.md)
