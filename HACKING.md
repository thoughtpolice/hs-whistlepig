# Notes for hackers

The current copy of Whistlepig is: **0.12, released 2012-06-09**

To update the copy of Whistlepig under src/cbits, just do:

    $ make update

You'll need `wget` on your `$PATH`.

The `src/cbits/whistlepig` directory will now contain an updated copy
of all the files needed to build. Be sure to update the `.cabal` file if
files are added/removed.
