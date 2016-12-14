# 男子漢
[要成為男子漢，不認輸](https://www.youtube.com/watch?v=LcWCG3K2BLk)

(a training data generator for images of Chinese characters)


## Installation

You will need [stack](https://docs.haskellstack.org/en/stable/README/).

Then run 

```
git clone https://github.com/MarkMcCaskey/nanzihan
cd nanzihan
stack setup
stack install
```


Be sure to add `~/.local/bin` to your `PATH`.


## Autocompletion from shell

For auto-complete of nanzihan from your shell add `eval "$(nanzihan --bash-completion-script nanzihan)"` to your `.bashrc`.

For shell auto-completion from ZSH add the following to your `.zshrc`:


```
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(nanzihan --bash-completion-script nanzihan)"
```
