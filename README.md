# bannedit
Emacs minor mode to highlight words from a 'banned words' list so you can dispatch them with extreme prejudice.

This mode was inspired by  by this post by Nat Eliason: https://www.nateliason.com/blog/better-writer 

The idea is to highlight commonly used weak words for easy identification. The 'bannedit-banned-words' can be words or phrases. Hihglighting of banned words is dynamic, quickly making you aware of overuse of words and phrases that weaken your prose.

With bannedit-mode enabled, toggle highlighting with `bannedit-toggle-bannedit`.

You can customize the face used to highlight banned words or edit the banned words list with `bannedit-face` and `bannedit-words`.

## Install

### Spacemacs
Clone this repo into `~/.emacs.d/private/local/` and add `(bannedit :location local)` to `dotspacemacs-additional-packages`

Then in `dotspacemacs-user-config` add `(require 'bannedit)`.

### Otherwise
Clone this repo and add `(add-to-list 'load-path "~/path/to/bannedit/")` to your config. Then `(require 'bannedit)`.
