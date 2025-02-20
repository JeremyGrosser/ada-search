This is the source for search.ada-lang.io. I'm aware that there are a lot of bad ideas and worst practices here. I don't want your advice, criticism, or feedback about design decisions herein. Send patches instead.

# Python version
Currently in production, this is a quick proof of concept I wrote for quickly searching the source code of alire crates.

- `backup.sh` uses the `alr get --only` command to download the source for the latest version of every crate in the index. It takes some time and gigabytes of disk space to run.
- `build_index.py` inserts the text of every .ads and .adb file in the downloaded sources into a SQLite FTS5 index along with some metadata
- `server.py` is the HTTP server/app. It uses pygments to perform source highlighting.

# Ada version
This is a rewrite I did in Ada to replace the Python version. A lot of the code is very rough and not up to my own standards. The idea was to define rough interfaces for all of the functionality I need and iterate on the implementation details later.

The HTTP server implementation is very experimental. No, I don't want to use your favorite library.

The code on the master branch shells out to `pygmentize` for source highlighting. This has some performance implications that are unacceptable in a production application. The `native-highlight` branch reimplements `Codesearch.Syntax` using libadalang's parser. It is very slow and I've not done a lot of digging as to why. It's probably allocating/copying memory somewhere it shouldn't.

# License
AGPL-3.0-or-later for now. I might switch to a more liberal license later.
