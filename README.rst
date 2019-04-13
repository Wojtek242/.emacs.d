.emacs.d
========

My personal Emacs configuration.

Installation
------------

Just clone this repository into your home directory:

::

   git clone https://github.com/Wojtek242/.emacs.d ~/.emacs.d

This configuration uses the `Source Code Pro`_ font.  If you do not have it
installed and it isn't available through your distribution you can install
the font by running

::

   git clone https://github.com/adobe-fonts/source-code-pro.git --branch release ~/.local/source-code-pro
   sudo cp ~/.local/source-code-pro/OTF/*.otf /usr/local/share/fonts

After cloning the repository and waiting for all packages to install on the
first startup you will also have to run the following command from within Emacs
in order to get all the icons used by the modeline

::

   M-x all-the-icons-install-fonts

External Support for Programming Languages
------------------------------------------

- Rust:

  Install RLS using ``rustup``

::
   rustup component add rls rust-analysis rust-src

- Python:

  The python configurations assumes ``python3`` and ``ipython3`` is installed

  Furthermore, install ``jedi``, ``autopep8``, ``flake8`` using ``pip3``

  Install the language server:

::
   pip3 install 'python-language-server[all]'

- C/C++:

  Uses ``ccls`` as the language server

::
   apt install clang libclang-dev
   git clone --depth=1 --recursive https://github.com/MaskRay/ccls
   cd ccls
   cmake -H. -BRelease
   cmake --build Release

  And make sure ``Release/ccls`` is in the ``$PATH``

Package Management
------------------

This ``.emacs.d`` uses its own small framework for package management located
in the ``emodule`` directory.  Its operation is heavily inspired by Spacemacs_,
but is much smaller with fewer features and thus simpler.

The purpose of using such a package manager is to ensure that all packages are
always installed before any configuration happens.  Furthermore, it will also
remove all unused packages and manage upgrades and backups.

Package configuration happens in the ``modules`` directory.  A file called
``MODULE.el`` corresponds to an individual ``MODULE``.  Inside each module
file, all the pre-requisite packages must be defined in a variable called
``emodule/MODULE-packages`` and all package configuration must be done in a
single function called ``emodule/MODULE-init``.  Each module must also be added
to the ``emodule/init`` call in the ``init.el`` file.

The ``emodule`` package manager runs during every startup. It will first ensure
all packages are installed before it runs any ``init`` functions so it is safe
to write configuration for packages that depends on other packages without any
concerns about ordering. This applies even if the configuration is spread
across multiple modules.

When Emacs is first launched with this configuration it will download all
the ``Elpa`` and ``Melpa`` packages that it needs so the first startup takes
much longer to complete.

Upgrades are supported and can be initiated either from the home screen or by
calling ``emodule/upgrade``.  I don't like tracking ``elpa`` with git as it
introduces a lot of code churn and makes analysing diffs, especially between
distant commits, very difficult. Instead, on each upgrade the ``elpa``
directory will be backed up to ``elpa.tar.xz``.  You can restore the last
backup by calling ``emodule/restore``. This repository should always contain a
functional backup of ``elpa``.

Theme
-----

The theme in use started with the ``underwater`` theme and has been heavily
modified, with inspiration from the Spacemacs_ theme, to add support for all
new faces introduced by the installed packages.  The code setting up the theme
is quite messy.  It is my plan to simplify it one day.

The theme files are located in ``themes``.

Stability
---------

I have been using this configuration at work and at home for many months now
which helped flush many bugs.  It has been pretty reliable, but suffers from
the occasional glitches which I think are more due to the limitations of
Emacs and the packages used than errors in my configuration.

.. _Spacemacs: http://spacemacs.org/
.. _`Source Code Pro`: https://github.com/adobe-fonts/source-code-pro
