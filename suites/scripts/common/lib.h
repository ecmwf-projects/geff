# ---------------------------
# set up the environment
# ---------------------------

%includeonce <suite.h>

# geff model executable
export PATH=$suite_dir/bin:$PATH

# efi computations
PATH=$PATH:/perm/ma/ma9/efi

export PYTHONUNBUFFERED=1
