#HsCMS
A Yesod/Haskell based CMS

### Admins ###

To be able to use the blog system, you need to add an admin email in settings.yml. This *must* be a gmail account (an account using google auth), since that's what's used for authentication.

### License ###

Free for use under the BSD license.

### Problems setting up ###

This assumes you're using cabal-dev.

If you run into the problem of:

<pre>
    cabal-dev: user error (Warning: cannot determine version of /home/user/.cabal/bin/fake-ghc-cabal-dev
    :
    "== GHC Arguments: Start ==\n--numeric-version\n== GHC Arguments: End ==\n"
    cabal: ghcInvocation: the programVersion must not be Nothing
    )
    yesod: readProcess: cabal-dev "buildopts" (exit 1): failed
</pre>

Then it should be fixed by running `cabal install cabal-dev cabal-install cabal` while standing in the project folder. 
Thanks to [this stackoverflow question](http://stackoverflow.com/questions/13659011/ghcinvocation-the-programversion-must-not-be-nothing) for the solution.
