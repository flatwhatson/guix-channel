# flat's guix channel

This is a personal collection of [GNU Guix][guix] package definitions.  Refer
to the manual for more information on [Guix Channels][guix-channel].

## Packages

### emacs-native-comp

Emacs built from the experimental `feature/native-comp` branch, which adds
support for native compilation of Elisp.

See [GccEmacs][gccemacs] for more information.

### libgccjit-10

An updated `libgccjit` package based on `gcc-10`.  The `libgccjit` package in
Guix is based on `gcc-9`, which is missing some changes that are important for
`emacs-native-comp` performance.

## Usage

The simplest and safest way to use this channel is to clone it somewhere and
add it to Guix's load-path when you need it:

``` shell
git clone https://github.com/flatwhatson/guix-channel.git
guix install -L ./guix-channel emacs-native-comp
```

## License

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.

See [COPYING](COPYING) for details.

[guix]: https://guix.gnu.org/
[guix-channel]: https://guix.gnu.org/manual/en/html_node/Channels.html
[gccemacs]: https://www.emacswiki.org/emacs/GccEmacs
