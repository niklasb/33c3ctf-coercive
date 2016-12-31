Setup
===============

To get the code:

    $ git clone git@github.com:niklasb/coercive
    $ cd coercive

Running
===============

We use docker for easy deployment:

    $ docker build -t niklasb/coercive .
    $ docker run -p 7777:7777 -it niklasb/coercive

Press ^P^Q to detach. The web server is running on port 7777. Temporary files
that are older than 5 minutes will be deleted regularly.

The deployment can update itself via git. For that it uses a deployment
SSH key to pull from git@github.com:niklasb/coercive.

TODO
===============

* Revisit interpolation logic in Report.hs to close any potential security
  holes
* Upgrade to GHC 8.0! 7.8 and 7.9 have seem to have [some serious problems
  with the type checker](https://ghc.haskell.org/trac/ghc/query?status=closed&version=7.9&or&version=7.8.4-rc1&group=resolution&col=id&col=summary&col=version&col=owner&col=type&col=priority&col=component&order=priority). Hopefully nobody looks at this too closely.
