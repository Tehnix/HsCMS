# HsCMS
* [Setup](#setup)
* [Authentication](#authentication)
* [Deployment](#deployment)
* [Settings](#settings)


## Setup
I assume you are somewhat familiar with the [yesod](http://www.yesodweb.com) framework, and have it installed. Else, you can do `cabal install yesod-platform` and `cabal install yesod-bin` to get going (or use the cabal sandbox feature).

First copy the `config/default-settings.yml` file, rename it to `config/settings.yml` and then set it up following the information under [settings](#settings). After this, do the same with `config/default-sqlite.yml` to `config/sqlite.yml`.

From there on out, it works as a simple [yesod web app](http://www.yesodweb.com) (it uses the [scaffold structure](http://www.yesodweb.com/book/scaffolding-and-the-site-template)).

Oh, and you probably also want a favicon. You get that by placing your favicon at `config/favicon.ico` and it should pick it up.


## Authentication
Only google authentication is used throughout the system, and therefore a google account is needed to use the admin facilities.

The reason for this is quite simply that I'd prefer not to have any login credentials or other things stored on the server, and would rather move that concern to a third-party (in this case google).


## Deployment
The preferred method of deployment is using the tool called [keter](https://github.com/snoyberg/keter). There is included a standard `config/default-keter.yaml` settings file in the root directory, same as with `config/settings.yml`, this needs to be copied and renamed to `config/keter.yaml`. 

Alter the settings to what you need, and then you can generate a `keter` bundle `yesod keter`. You can then move this to where your `keter` service expects it to be (you can also use the `copy-to` feature in the `keter.yaml` file to do this last step automatically).

If you need to run `keter` alongside `nginx`, there is also a good  [guide for that here](https://github.com/yesodweb/yesod/wiki/Deploying-via-Keter-alongside-Nginx).

## Keep the same SQLite3 database through deployments
To keep the same SQLite3 database file throughout deployments, you need to set its path to a shared destination. My own approach, when using keter, is to make a directory at `/opt/keter/database`, and then set the setting for the production sqlite3 file to,


```
Production:
  database: /opt/keter/database/HsCMS_production.sqlite3
  poolsize: 100
  <<: *defaults
```


## Heroku

__Not quite working yet...__

Following the information in the Procfile, we first move the Procfile into the root directory,
```
    mv deploy/Procfile .
```

Then we create a json package file for heroku,
```
    echo '{ "name": "HsCMS", "version": "0.0.1", "dependencies": {} }' >> package.json
```

If this is the first time, you need to create the heroku app while specifying the stack and the buildpack,
```
    heroku create yesod-codetalk --stack cedar --buildpack https://github.com/begriffs/heroku-buildpack-ghc.git
```

the buildpack was picked from herokus list of [third-party buildpacks](https://devcenter.heroku.com/articles/third-party-buildpacks)

There's some general information about to to use the buildpack on the [GitHub repository page](https://github.com/begriffs/heroku-buildpack-ghc), but you will probably hit the 15 minute build time limit. To get past that, we use an alternative build process,
```
    heroku plugins:install https://github.com/ddollar/heroku-anvil
    heroku build -r -b https://github.com/begriffs/heroku-buildpack-ghc.git
```

After that you should be good to go.


## Settings
There are several settings that allow you to customize the system a bit.

#### Required 
```
    admins: ["exampleadmin@gmail.com", "otheradmin@gmail.com"]
```
The `admins` settings is a list of all the emails that are allowed in the admin section of the system. Since google auth is used, it's impossible to gain access without owning or having control over the defined email.

#### Optional 
```
    disqus: your_disqus_shortname
```
The `disqus` setting allows you to set it to your disqus name, and, via that, integrate comments on the article pages.


```
    githubToken: your_github_personal_access_key
```
The `githubToken` setting is really a novelty feature. It allows for automatic gist creation of your blog posts, and also updates them when you update your posts, effectively giving you a history of revisions. 

The gist is created the first time you publish your post, and is updated everytime you update the post. It isn't deleted or hidden if you later on unpublish your post.


```
    cloudflareKey: cloudflare_api_key
```
__Under development:__ Will allow HsCMS to pull visitor statistics and show them in the dashboard. 