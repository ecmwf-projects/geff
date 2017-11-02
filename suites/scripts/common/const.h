# Header file for tasks which depend on the
# 'constant' forcing fields (land-sea mask,
# vegetation cover, vegetation stage, slope).

# The actual header file is provided by the
# context in which the task is running.

%includeonce <const_%CONTEXT%.h>
