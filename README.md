# HsCMS
HsCMS is a small blogging platform. It allows you to easily setup and run your own blog, by taking care of the administration details and the back-end.

What is has:
* Markdown editing for articles and static pages with live preview
* Automatic gist creation (which is actually nice for backup)
* Integrates with disqus for comments
* Can pull cloudflare analytics info (and also disqus analytics info)

If you want to follow the progress you can check the [issues](https://github.com/Tehnix/HsCMS/issues?state=open "HsCMS Issues") section of the project.

For any additional information, check out the [wiki pages](https://github.com/Tehnix/HsCMS/wiki/_pages "Wiki Pages") (although some information is also repeated here in the readme).

Contributions are more than welcome! :)...

* [Setup](#setup)
* [Authentication](#authentication)
* [Deployment](#deployment)
* [Custom Layouts](#custom-layouts)
* [Settings](#settings)


## Setup
I assume you are somewhat familiar with the [yesod](http://www.yesodweb.com) framework.

The steps to get going are:
* copy the `config/default-settings.yml` file, rename it to `config/settings.yml` and then set it up following the information under [settings](#settings).
* do the same with `config/default-sqlite.yml` to `config/sqlite.yml`.

From there on out, it works as a simple [yesod web app](http://www.yesodweb.com) (it uses the [scaffold structure](http://www.yesodweb.com/book/scaffolding-and-the-site-template)).

Oh, and you probably also want a favicon. You get that by placing your favicon at `config/favicon.ico` and it should pick it up.


## Authentication
Only google authentication is used throughout the system, and therefore a google account is needed to use the admin facilities.

The reason for this is quite simply that I'd prefer not to have any login credentials or other things stored on the server, and would rather move that concern to a third-party (in this case google).


## Deployment
The preferred method of deployment is using the tool called [keter](https://github.com/snoyberg/keter). There is included a standard `config/default-keter.yaml` settings file in the root directory, same as with `config/settings.yml`, this needs to be copied and renamed to `config/keter.yaml`.

Alter the settings to what you need, and then you can generate a `keter` bundle `yesod keter`. You can then move this to where your `keter` service expects it to be (you can also use the `copy-to` feature in the `keter.yaml` file to do this last step automatically).

If you need to run `keter` alongside `nginx`, there is also a good  [guide for that here](https://github.com/yesodweb/yesod/wiki/Deploying-via-Keter-alongside-Nginx).

#### Keep the same SQLite3 database through deployments
To keep the same SQLite3 database file throughout deployments, you need to set its path to a shared destination. My own approach, when using keter, is to make a directory at `/opt/keter/database`, and then set the setting for the production sqlite3 file to,


```
Production:
  database: /opt/keter/database/HsCMS_production.sqlite3
  poolsize: 100
  <<: *defaults
```


## Custom Layouts
HsCMS provides a default layout you can use to get quickly started. If you want to use another layout, it is recommended that you don't alter the default layout, but create your own separate one.

To do this:

* Create a folder in `templates/layouts` with the name of your layout
* Make sure it has, as minimum, a `layout.hamlet` and a `wrapper.hamlet` file
* Add `layout: "my-new-layout"` to `config/settings.yml`, where `my-new-layout` is the name of the folder you created earlier

Because of how this currently functions, layouts are not autorecompiled when using `yesod devel` meaning you have to stop the devel command and execute it again.


## Settings
There are several settings that allow you to customise the system a bit.

#### Required
```
    admins: ["exampleadmin@gmail.com", "otheradmin@gmail.com"]
```
The `admins` settings is a list of all the emails that are allowed in the admin section of the system. Since google auth is used, it's impossible to gain access without owning or having control over the defined email.

#### Optional
__Disqus__

```
    disqus: your_disqus_shortname
    disqusSecretKey: your_disqus_secret_api_key
    disqusAccessToken: your_disqus_access_token
```
The `disqus` setting allows you to set it to your disqus name, and, via that, integrate comments on the article pages. I recommend having different disqus sites for development and for production.

If the `disqusSecretKey` `disqusAccessToken` are set, you can view comment statistics in the admin dashboard. They are completely optional, and only the `disqus` setting is needed for comments on your site.


__GitHub Gists__

```
    githubToken: your_github_personal_access_key
    gistPublic: false
```
The `githubToken` setting is really a novelty feature. It allows for automatic gist creation of your blog posts, and also updates them when you update your posts, effectively giving you a history of revisions and online backup of blog posts.

The gist is created the first time you publish your post, and is updated every time you update the post. It isn't deleted or hidden if you later on unpublish your post.

If `gistPublic` is false, it will post as a secret gist, if not, it will be public and the creation along with updates will be shown in the feeds of the users that follow you on GitHub.

NOTE: you might not want to set `githubToken` in the development section, but only in production, so you don't end up with a lot of useless test gists.


__CloudFlare__

```
    cloudflareKey: cloudflare_api_key
    cloudflareMail: "mail_associated_with_api_key@mail.com"
    cloudflareZone: "example.com"
```
Allows HsCMS to pull visitor statistics and show them in the dashboard.


__Layouts__

```
    layout: "my-new-layout"
```

As mentioned in the [section about layouts](#custom-layouts), this is used to set another layout than the default-layout. If it is not present, HsCMS will use the layout found in `templates/default-layout`.
