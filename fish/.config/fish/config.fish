if status is-interactive
    # Commands to run in interactive sessions can go here
end

function ..
    cd ..
end
function ...
    cd ../..
end
function ....
    cd ../../..
end
function .....
    cd ../../../..
end

set -x JAVA_HOME (/usr/libexec/java_home -v 25)
set -x PATH $JAVA_HOME/bin $PATH

source "$HOME/.cargo/env.fish"

zoxide init fish | source
starship init fish | source
