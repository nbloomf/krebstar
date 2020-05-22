krebdoc is an opinionated tool for turning a pile of markdown files into html.



How to use it
-------------

Everything krebdoc does is with respect to a _project root directory_. By default this is the present working directory the program was invoked within. (there is an option to set this at the command line but it is broken.)

The project root directory must have the following structure (there can be other stuff too, but this is required):

root
+-krebdoc.config.json
+-aux
  +-doc.html5
  +-styles
    +-style.css

krebdoc.config.json is a -- wait for it -- json configuration file for krebdoc. As of version 0.0 it is an object with two required keys:

- configVersion: Version of the config spec the file conforms to. must be exactly "0.0".
- sources: a list of file paths relative to the project root directory. These can include basic globs and wildcards.

    - ? matches exactly one character
    - * matches zero or more characters
    - [list] matches any character in list
    - [!list] matches any character not in the list

doc.html5 is a pandoc template file. It should include at least the $body key or things will be pretty boring.

style.css is where your stylesheet goes. use it wisely because you only get one.



What it does
------------

krebdoc runs pandoc over all directories specified in the sources list of krebdoc.config.json, converting them to html, and writing them to a parallel directory hierarchy under docs/html. A handful of nice reader extensions are enabled (can't turn them off, sorry): literate_haskell, inline_notes, yaml_metadata_block, fenced_divs, fenced_code_blocks, and fenced_attributes.

krebdoc also runs the source files through some custom pandoc filters to do some other goodies, much of which is ripped off from other sources. The extra goodies are as follows:

- Inline notes are converted to "sidenotes" compatible with the tufte css. if you want inline notes you should use tufte css or it will look bad, sorry.

- Fenced divs named 'tikz' are compiled with pdflatex, converted to svg by inkscape, and linked in the document. image files are written to docs/img/tikz.

- You can do really basic (named) cross references. Set an anchor point with [@slug]() and refer to it with [text](@slug), where '@slug' is the reference name and 'text' is your link text. The reference slug must consist only of a-zA-Z0-9 and -_ to be recognized as such.



What it doesn't do
------------------

krebdoc is not clever about only rebuilding files that changed; when you run it it rebuilds everything from scratch. The way cross references work mean this kind of optimization probably isn't possible anyway.
