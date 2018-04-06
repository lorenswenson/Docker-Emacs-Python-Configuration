#!/usr/bin/env sh

docker_func () {
    ipython3 \
        --matplotlib='tk' \
        --TerminalIPythonApp.interactive_shell_class=rlipython.TerminalInteractiveShell \
        $@
}
#           --simple-prompt \
#           continuumio/anaconda3 ipython3 \
#--TerminalInteractiveShell.display_completions='readlinelike' \

docker_func $@ # this will work.
