# pdf-slave-server

Web server for `pdf-slave` that renders posted templates.

# Building

```
stack setup
stack build
```

# Debug deploy

```
pdf-slave-server --conf config.yaml
```
Note: default login/password is `admin/123456`

# Production deploy

The repo has scripts to cook deb packages for Ubuntu 16. Simply run `makedeb.sh`, you need [fpm](https://github.com/jordansissel/fpm) installed. The simple way to install it is:

``` bash
dnf install ruby-devel gcc make rpm-build libffi-devel  # Fedora (yep, i'm biased)
# apt-get install ruby ruby-dev rubygems build-essential # Ubuntu
gem install --no-ri --no-rdoc fpm
```
