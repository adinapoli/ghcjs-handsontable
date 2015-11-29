# GHCJS Handsontable

This is a playgrond to:

1. Get myself acquainted with GHCJS and its FFI
2. Port a subset of [Handsontable](http://handsontable.com) to Haskell.

## Building & Testing

```
sh build.sh
open demo/index.html
```

## File watching

You will need [entr](http://entrproject.org/) for this.
(We cannot simply use `stack build --file-watch` as we also need to copy
the generated `all.js` in the `demo` folder, suggestions welcome!)

```
sh watch.sh
```
