# Signal handling for ecFlow bash tasks.

# Env. variables needed by 'ecflow_client'
# to communicate with ecFlow server.
export ECF_PORT=%ECF_PORT%
export ECF_NODE=%ECF_NODE%
export ECF_NAME=%ECF_NAME%
export ECF_PASS=%ECF_PASS%

echo $HOST

module load ecflow/%ECF_VERSION%

set -x   # Print every line as it is executed
set -u   # Exit when there is an undefined variable
         # We don't use "set -e". Instead we trap ERR signal.

# Tell ecFlow server we have started
ecflow_client --init=$$


# Code to be called on exit
__ecflow_bash_exit() {

    __ecflow_bash_exit_status=$?

    # wait for child processes
    wait

    # remove temporary directories
    if [[ -n $TMPDIR ]]; then
        rm -rf $TMPDIR || :
    fi
    if [[ -n $SCRATCHDIR ]]; then
        rm -rf $SCRATCHDIR || :
    fi

    # print elapsed time
    echo TASK DURATION $(($(date +%%s) - $TASK_START_TIME))

    # notify ecFlow server that we are finished
    if [[ $__ecflow_bash_exit_status -ne 0 ]]; then
        trap - EXIT
        ecflow_client --abort
    else
        ecflow_client --complete
    fi
    exit $__ecflow_bash_exit_status
}


trap '__ecflow_bash_exit' EXIT ERR HUP QUIT INT TERM

TASK_START_TIME=$(date +%%s)


if [[ ${BASH:-no} == no ]]; then
    echo "ecflow_bash.h needs bash interpreter. Aborting."
    false
fi

cd $TMPDIR

