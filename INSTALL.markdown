Dependencies
------------

Install distro supplied development packages for Pango and Cairo:

### Fedora

    # dnf install -y \
        pango-devel \
        cairo-devel

### Haskell

In addition to running `stack build` in your project, you will need
the _pandoc_ executable supplied by the **pandoc** package.

    $ stack install pandoc

