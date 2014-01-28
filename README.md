# HsCMS
* [Setup](#setup)
* [Authentication](#authentication)
* [Deployment](#deployment)
* [Settings](#settings)


## Setup
I assume you are somewhat familiar with the [yesod](http://www.yesodweb.com) framework, and have it installed. Else, you can do `cabal install yesod-platform` and `cabal install yesod-bin` to get going (or use the cabal sandbox feature).

Simply copy the `config/default-settings.yml` file, rename it to `config/settings.yml` and then set it up following the information under [settings](#settings).

From there on out, it works as a simple [yesod web app](http://www.yesodweb.com) (it uses the [scaffold structure](http://www.yesodweb.com/book/scaffolding-and-the-site-template)).


## Authentication
Only google authentication is used throughout the system, and therefore a google account is needed to use the admin facilities.

The reason for this is quite simply that I'd prefer not to have any login credentials or other things stored on the server, and would rather move that concern to a third-party (in this case google).


## Deployment
The preferred method of deployment is using the tool called [keter](https://github.com/snoyberg/keter). There is included a standard `config/default-keter.yml` settings file in the root directory, same as with `config/settings.yml`, this needs to be copied and renamed to `config/keter.yml`. 

Alter the settings to what you need, and then you can generate a `keter` bundle `yesod keter`. You can then move this to where your `keter` service expects it to be (you can also use the `copy-to` feature in the `keter.yml` file to do this last step automatically).

If you need to run `keter` alongside `nginx`, there is also a good  [guide for that here](https://github.com/yesodweb/yesod/wiki/Deploying-via-Keter-alongside-Nginx).


## Settings
There are several settings that allow you to customize the system a bit.

#### Required 
<pre>
    admins: ["exampleadmin@gmail.com", "otheradmin@gmail.com"]
</pre>
The `admins` settings is a list of all the emails that are allowed in the admin section of the system. Since google auth is used, it's impossible to gain access without owning or having control over the defined email.


<pre>
    theme: default
</pre>
__(under development)__ The `theme` setting allows you to use your own custom theme for the front-end, instead of the prepackaged ones. To make a new theme, place a folder at `templates/theme/your_theme_name/` and change the theme value to `your_theme_name`. 

This assumes that you've implemented all the files that are used (the compiler will tell you if some are missing).


#### Optional 
<pre>
    disqus: your_disqus_shortname
</pre>
The `disqus` setting allows you to set it to your disqus name, and, via that, integrate comments on the article pages.


<pre>
    githubToken: your_github_personal_access_key
</pre>
The `githubToken` setting is really a novelty feature. It allows for automatic gist creation of your blog posts, and also updates them when you update your posts, effectively giving you a history of revisions. 

The gist is created the first time you publish your post, and is updated everytime you update the post. It isn't deleted or hidden if you later on unpublish your post.