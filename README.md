# bannedit
Emacs minor mode to highlight words from a 'banned words' list so you can dispatch them with extreme prejudice.

This mode was inspired by Nat Eliason's post giving [21 tips to become a better writer](https://www.nateliason.com/blog/better-writer). One in particular stood out to me -- keeping a banned words list. Maintaing a banned words list and striving to replace or simply remove those words is a fast and easy way to punch any piece of writing.

The idea is to highlight commonly used weak words for easy identification. The 'bannedit-banned-words' here can be words or phrases; for example "I think" or "to be." Highlighting banned words is dynamic, quickly making you aware of overused words and phrases that weaken prose. Now you can easily eradicate them.

With bannedit-mode enabled, toggle highlighting with `bannedit-toggle-bannedit`.

You can customize the face used to highlight banned words or edit the banned words list with `bannedit-face` and `bannedit-words`.

## Screencast

![](screencast.gif)

## Installation

### Spacemacs
Clone this repo into `~/.emacs.d/private/local/` and add `(bannedit :location local)` to `dotspacemacs-additional-packages`

Then in `dotspacemacs-user-config` add `(require 'bannedit)`.

### Otherwise
Clone this repo and add `(add-to-list 'load-path "~/path/to/bannedit/")` to your config. Then `(require 'bannedit)`.

## Related packages
After having created this package, several helpful commenters have alerted me to the existence of some very nice, much more fully-featured packages:

[writegood-mode](https://github.com/bnbeckwith/writegood-mode) offers a similar feature with `writegood-weasel-words` plus lots of other helpful tools.

flycheck can also use the [proselint](https://github.com/amperser/proselint) linter to provide all kinds of warnings for weasel words, offensive terms, jargon, cliches, and more.

## Credits
Credit to [protesilaos](https://github.com/protesilaos) for the gorgeous [modus-operandi theme](https://gitlab.com/protesilaos/modus-themes) shown in the gif above, as well as for some helpful advice on working with faces.
