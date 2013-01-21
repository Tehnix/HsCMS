#HsCMS
A Yesod/Haskell based CMS


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
