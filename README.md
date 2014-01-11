Only google auth is used throughout the system, and therefore a google account is needed to use the admin facilities.

## Settings ##
There are several settings values that allow you to customize the system.

### Required ###
<pre>
    admins: ["example@gmail.com", "otheradmin@gmail.com"]
</pre>
The `admins` settings is a list of all the emails that are allowed in the admin section of the system. Since google auth is used, it's impossible to gain access without owning or having control over the defined email.


<pre>
    theme: default
</pre>
__(NOT WORKING YET)__ The `theme` setting allows you to use your own custom theme for the front-end, instead of the prepackaged ones. To make a new theme, place a folder at `templates/theme/your_theme_name/` and change the theme value to `your_theme_name`. This assumes that you've implemented all the files that are used (the compiler will tell you if some are missing).


### Optional ###
<pre>
    disqus: your_disqus_shortname
</pre>
The `disqus` setting allows you to set it to your disqus name, and, via that, integrate comments on the article pages.


<pre>
    githubKey: your_github_personal_access_key
</pre>
__(NOT WORKING YET)__ The `githubKey` setting is really a novelty feature. It allows for automatic gist creation of your blog posts, and also updates them when you update your posts, effectively giving you a history of revisions. The gist is created the first time you publish your post, and is updated everytime you update the post. It isn't deleted or hidden when if you later on unpublish your post, but is deleted when you deleted your post.