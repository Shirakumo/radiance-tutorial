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
* [CodeMirror](http://codemirror.net/doc/manual.html)
* [Ubiquitous](https://shinmera.github.io/ubiquitous)

## A Short Roadmap
This is the last part that deals with actual development of our application. The final part is one from the perspective of an administrator.

* A capable editor with syntax highlighting
* Missing API endpoints
* Configuration and user settings

Let's wrap up our development, then.

## Syntax Highlighting
What we've been missing all this time is syntax highlighting. I've avoided this topic, because it's something that is much more on the side of the client than on the side of the server. Indeed, we're going to use an already available JavaScript solution to do the markup and editing support for us for the most part. We're going to employ the help of [CodeMirror](http://codemirror.net/), which is an excellent editor solution.

However, in order to properly be able to mark up the text, we need a new field on our pastes to track what kind of data it is supposed to be. Schema update time. A varchar field of length 32 should suffice. We also need to update the `create-paste` and `edit-paste` functions, as well as the API endpoints to supply these parameters. At this point, doing so should not be an issue for you on your own.

One thing we definitely should go into though is what values are allowed for the field. After all, we will probably want to offer a selector to the user so that they can pick an available mode, rather than having to type it in manually. The simplest way of going about this would be to define a list of all known modes that CodeMirror supports. That's a bad idea however for multiple reasons, so instead I'll go another way. Namely, we'll automatically gather the information from the CodeMirror distribution.

The first step towards this is to grab a copy of CodeMirror. We'll use a cleaned version here that has all of the extra files stripped away. Download [a release](https://github.com/Shinmera/codemirror-dist/releases/) and extract it into `plaster/static/codemirror/`. Now that we have this, we can add some code to read out all the modes.

```common-lisp
(defparameter *paste-types*
  (list* "text"
         (sort (mapcar #'pathname-name
                       (uiop:directory-files (@static "codemirror/mode/")))
               #'string<)))
```

Naturally, this is not *perfect*. The file names are not properly capitalised and some of them are strung-together words. If you want to go the extra mile, you can define a manual list of labels to mode names. For the sake of brevity and maintainability, I will forsake this here. Now we just need another validity check in `create-paste` and `edit-paste`.

```common-lisp
(defun ensure-paste-type (type)
  (let ((type (string-downcase (or type "text"))))
    (unless (find type *paste-types* :test #'string=)
      (error 'api-argument-invalid :argument "type"
                                   :message (format T "Type must be one of ~{~s~^, ~}." *paste-types*)))
    type))
```

That should do nicely. The default type here is `"text"`, to denote no particular encoding. We'll need to handle that case specially too, since we won't want to load any mode at all for it. Before we get to actually including the editor though, let's allow the user to specify the type. Another element in the edit header should suffice.

```HTML
<select class="type" name="type" iterate="(** :types)">
  <option lquery="(text *) (attr :value * :selected (equal * (** :type)))">TYPE</option>
</select>
```

And of course, we'll also want to display it in the view page's header as well.

```HTML
<span class="type" lquery="(text type)">TYPE</span>
```

Alright then, time to write some JavaScript. What we're writing will need to be able to read out all of the edit forms in the page, load the appropriate modes, and initialise CodeMirror for each instance. In the case of the edit page, it even needs to be able to handle on-the-fly mode switching when the user selects a different mode.

Personally I subscribe to the idea of using as little JavaScript as possible. As such, we won't be using any special tools to process or generate JS, nor will we be using any libraries outside of CodeMirror itself. Whatever the case, for the little our script actually needs to do, it is going to be rather verbose. I'll paste it all here and go through the interesting tidbits after.

```javascript
var Plaster = function(){
    var self = this;

    self.loadedModes = ["text"];
    self.loadedThemes = ["default"];
    self.staticUrl = "/static/plaster/codemirror/";
    self.editors = [];
    self.defaultConfig = {"lineNumbers": true,
                          "lineWrapping": true,
                          "viewportMargin": Infinity,};

    self.addToHead = function(element){
        document.getElementsByTagName("head")[0].appendChild(element);
        return element;
    }

    self.loadScript = function(url, callback){
        var script = document.createElement("script");
        script.type = "text/javascript";
        script.src = url;
        script.onload = callback;
        return self.addToHead(script);
    }

    self.loadStylesheet = function(url, callback){
        var link = document.createElement("link");
        link.type = "text/css";
        link.rel = "stylesheet";
        link.href = url;
        link.onload = callback;
        return self.addToHead(link);
    }

    self.maybeLoadTheme = function(theme, callback){
        if(self.loadedThemes.indexOf(theme) === -1){
            self.loadedThemes.push(theme);
            self.loadStylesheet(self.staticUrl + "theme/"+theme+".css", callback);
        }else{
            if(callback) callback();
        }
        return true;
    }

    self.maybeLoadMode = function(mode, callback){
        if(self.loadedModes.indexOf(mode) === -1){
            self.loadedModes.push(mode);
            self.loadScript(self.staticUrl + "mode/"+mode+".js", callback);
        }else{
            if(callback) callback();
        }
        return true;
    }

    self.createEditor = function(element, config, callback){
        var textarea = element.getElementsByTagName("textarea")[0];
        var type = element.getElementsByClassName("type")[0];

        self.editors.push(element);
        if(!config) config = {};
        if(!config.theme) config.theme = "default";
        if(!config.readOnly) config.readOnly = textarea.hasAttribute("readonly");
        
        if(type.tagName === "select"){
            config.mode = type.options[type.selectedIndex].value;
            type.addEventListener("change", function(){
                self.changeMode(element, type.options[type.selectedIndex].value);
            }, false);
        }else{
            if(!config.mode) config.mode = type.textContent;
        }

        element.mirror = null;
        self.maybeLoadTheme(config.theme, function(){
            self.maybeLoadMode(config.mode, function(){
                textarea.removeAttribute("required");
                element.mirror = CodeMirror.fromTextArea(textarea, config);
                if(callback) callback(element);
            });
        });
        return element;
    }

    self.maybeCreateEditor = function(element, config, callback){
        if(self.editors.indexOf(element) === -1){
            self.createEditor(element, config, callback);
        }else{
            if(callback) callback();
        }
        return element;
    }

    self.changeMode = function(element, mode){
        if(!element.mirror) throw element+" is not an initialized CodeMirror editor.";
        self.maybeLoadMode(mode, function(){
            element.mirror.setOption("mode", mode);
        });
        return element;
    }

    self.changeTheme = function(element, theme){
        if(!element.mirror) throw element+" is not an initialized CodeMirror editor.";
        self.maybeLoadTheme(theme, function(){
            element.mirror.setOption("theme", theme);
        });
        return element;
    }

    self.initEditors = function(){
        var els = document.getElementsByClassName("edit");

        var createNext = function(i){
            if(i<els.length)
                self.maybeCreateEditor(els[i], self.defaultConfig, function(){createNext(i+1)});
        }
        
        createNext(0);
        return els;
    }

    self.init = function(){
        self.initEditors();
        return true;
    }
}

var plaster = new Plaster();
document.addEventListener("DOMContentLoaded", plaster.init, false);
```

Some notes on idiosyncrasies of my code first. I use a variable `self` to consistently capture `this` within the scope of my own object. This is because the actual value of `this` is dynamically bound in JavaScript and can thus diverge from the actual object. To make sure this doesn't happen, I consistently use `self` everywhere. All of my functions are written to return some kind of value, even if it may not be particularly useful. This is mostly to emulate the same feel Lisp gives you, where everything evaluates to some kind of value. Finally, I haven't added much of any validity checking here at all because I wanted to keep things somewhat brief, still.

With that out of the way, let's actually see what this does. In order to avoid accidentally reloading modes or themes, I keep a list of already loaded parts for each. I also keep a list of known editor instances, and the base URL for the static part. If you understand what Radiance is about, that last part should ring some alarm bells. We'll get back to it later. CodeMirror has a lot of configurable options that you can set. I've set some reasonable defaults here that should give a good experience. See its [documentation](http://codemirror.net/doc/manual.html) for more information.

On to the functions. The first few-- `addToHead`, `loadScript`, `loadStylesheet`, `maybeLoadTheme`, and `maybeLoadMode` --are helpers responsible for modifying the DOM and loading resources dynamically. JS doesn't have an included way of loading resources dynamically, but adding the respective elements to the DOM turns out to work just fine. Note also that there's callbacks all over the place, which is an unfortunate reality we have to deal with in the asynchronous world of JS. Note also that I do not deal with race conditions here. It is entirely possible that a `maybeLoadMode` of the same mode is requested twice before it is fully loaded the first time, resulting in the callback being executed prematurely. Guarding against this would burst the length on this, so I won't indulge in it.

The `createEditor` function is definitely the most interesting one. It searches out the data it needs from the form's elements and sets the default config values accordingly. If the `type` element is a select, it knows that it might need to change dynamically and adds an event listener for that onto the element. Finally it uses nested callbacks to load the requested theme and mode before creating the actual CodeMirror instance. The removal of the `required` attribute here is a bug workaround for some browsers. CodeMirror hides the textarea, after which it can't be properly edited anymore with the required attribute due to an attempt to focus that fails in the browser. `maybeCreateEditor` just ensures that the same editor isn't made twice.

Finally, `changeMode` and `changeTheme` are thing wrappers around mode/theme loading and CodeMirror's `setOption` function that is actually responsible for switching over. `initEditors` is also interesting in that it uses an iterative version of a callback to initialise one editor after the other. While that is potentially much slower than an asynchronous version could be, it avoids the above mentioned race conditions by ensuring that only one mode/theme for each editor is loaded at once.

And lastly, the file just constructs a standard instance and registers its init function to be run once the DOM is all baked. Now it's time to add everything to the `view` and `edit` templates! Into the header we need to add another stylesheet link:

```HTML
<link rel="stylesheet" type="text/css" @href="/static/plaster/codemirror/codemirror.css" />
```

And just before the closing body tag we add some scripts:

```HTML
<script type="text/javascript" @src="/static/plaster/codemirror/codemirror.js" />
<script type="text/javascript" @src="/static/plaster/plaster.js" />
```

The reason why you want to add those at the bottom is that the browser can already start rendering the DOM before it has to load the JS files and evaluate them. That should lead to a much more responsive page, even if it appears without the CodeMirror highlighting for a bit. With that, everything is set to go. Note also that everything we've written so far will happily work without JavaScript enabled. It'll lack some of the pizzazz, but it'll still work just as fine.

But wait! What about that suspicious `staticUrl`? Indeed, that is not the proper way to go about this. It'll work out for your particular setup, but it'll cease to work as soon as you were to try and run this on a server where the root path is not available for whatever reason or indeed where static resources are in a completely different place altogether. In order to do this right, we need to ask Radiance's routing system for help, just like we did for all the links in the HTML templates so far.

Fortunately, we don't need to ask Radiance for help with each separate URL. It promises us that as soon as we have the proper external URL that points to the root static directory of a module, everything afterwards must stay the same. In other words, a transformation that turns a URI `/static/foo/bar/baz` into an URL like `http://cdn.com/something/else/bar/baz` is legal, but a transform that turns it into something like `http://cdn.com/something/baz` is not because the `bar/baz` part is not there verbatim. Thus, it suffices if Radiance tells us what `/static/plaster/codemirror/` translates into and we can just concatenate from there.

The best way to do this is to emit a placeholder element in the header of the template that just contains the link we want.

```HTML
<link id="static-codemirror-root" rel="alternate" @href="/static/plaster/codemirror/" />
```

We can then change that part of our JavaScript to:

```javascript
self.staticUrl = document.getElementById("static-codemirror-root").href;
```

Now it'll use a properly translated URL that should withstand any server setup.

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

## Configuration
I've briefly touched on Radiance's concept of an environment before. It is what decides the mapping between an interface and a particular implementation and is thus a vital part of any Radiance setup. However, it is more general than that particular mapping. The environment is a configuration storage system that can be used by any module. It is automatically backed to human-readable configuration files and requires no setup whatsoever.

We've already defined a couple of variables in our application that smell very much like "configuration". Namely the `*pastes-per-page*`, `*default-api-amount*`, `*maximum-api-amount*`, and `*password-salt*` variables. Replacing them with an access to the configuration system is simple enough.

Before we do that however, we should configure some sane defaults, just like we did with the variables. We can stuff these into the same trigger we've used to set the default permissions. While we're here, we should actually also outsource our default permission entries to the environment system. Why? Because if we don't, it becomes rather difficult for an administrator to enforce other default permissions than the one we think are sensible. By routing that through the environment, we can set sensible "first-time" defaults and still let the sysadmin do his work.

All things considered, our trigger should now look like this:

```common-lisp
(define-trigger user:ready ()
  (defaulted-config 25 :pastes-per-page)
  (defaulted-config 50 :api :default-amount)
  (defaulted-config 100 :api :maximum-amount)
  (defaulted-config (make-random-string) :password-salt)

  (defaulted-config (list
                     (perm plaster paste new)
                     (perm plaster paste view)
                     (perm plaster paste list)
                     (perm plaster paste user)
                     (perm plaster paste edit own)
                     (perm plaster paste delete own))
                    :permissions :default)

  (defaulted-config (list
                     (perm plaster paste new)
                     (perm plaster paste view)
                     (perm plaster paste list)
                     (perm plaster paste user))
                    :permissions :anonymous)

  (apply #'user:add-default-permissions (config :permissions :default))
  (apply #'user:grant "anonymous" (config :permissions :anonymous)))
```

The `defaulted-config` function sets the configuration value at the path to the first argument if it has not yet been set before, which is exactly what we need. We can then access the value with `config`. Both of these functions are thing wrappers around [Ubiquitous](https://shinmera.github.io/ubiquitous)' `defaulted-value` and `value` that take care of persisting the proper storage for our module. What's also nice about this is that we get a persistent, but random for each setup password salt automatically.

With the configuration set, we just have to exchange the references to our special variables with `(config ..)` calls. Your application should work just the same as it did before once you're done.

Finally, using the configuration has another advantage, in that the possible configuration variables show up in the module's description. If you run `(describe (radiance:module :plaster))` now, you should see our configuration paths in the output.

## User Settings


[Part 7](Part 7.md)
