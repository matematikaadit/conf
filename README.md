Configuration and Dotfiles
==========================

Repository of configruation and dotfiles.

Usage
-----

We will use this term in this project.

- **target**, the configuration files that lives in it's proper place.
- **source**, the source of the corresponding target that lives in this repo.

Here are few commands that could be run to automate some tasks.

- `make`,  for syncronizing the target and the source. The difference between
  both files will be merged and reflected in both files to make both identical.
  If there's conflict when merging, an error will be reported and the files
  will be kept intact. An alias for `make sync`
- `make diff`, for showing the diff of the conflicted files from running `make
  sync`.

Take a note that you'll need sudo if you're overwriting a target that only
writable by super user.

There's a convention for the ease of use of writing new configuartion. Here are
a few of them.

- The target of source under `_root` directory is the same path that
  represented by that source with `_root/` replaced by the real file system
  root, `/`.  For example, the target of the
  `_root/etc/nixos/configuration.nix` is `/etc/nixos/configuration.nix`.
- Everything else will have target path under `$HOME` directory, but with
  underscore in front of that path (if present) replaced by dot. For example,
  the target of `_vimrc` is `$HOME/.vimrc` and the target of
  `_xmonad/xmonad.hs` is `$HOME/.xmonad/xmonad.hs`.

License
-------

The MIT License (MIT)

Copyright (c) 2015 Adit Cahya Ramadhan

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

<!--
vim:ft=markdown:sw=4:sts=4:ai:et:bs=indent,eol,start:
-->
