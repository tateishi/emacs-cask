# emacs-cask
emacs init files using  Cask


# 初期設定

## cask のインストール

~/.cask に リポジトリをクローンする

- ubuntu / linux / mac
```bash
$ git clone https://github.com/cask/cask.git ~/.cask
```

- windows
```
> git clone https://github.com/cask/cask.git ~/.cask
```


## パッケージの初期設定

- ubuntu / linux / mac

```
$ git pull
$ cask update
$ cask install
$ cask upgrade
$ emacs

```

- windows

```
> git pull
> emacs -Q --script ~/.cask/cask-cli.el -- update
> emacs -Q --script ~/.cask/cask-cli.el -- install
> emacs -Q --script ~/.cask/cask-cli.el -- upgrade
> emacs
```

または

```
> git pull
> scripts/update.bat
```
