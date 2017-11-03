# version of the GEFF system
geff_version=2.0

# if expver not specified, use $geff_version
# as an identifier of this run in the archive.
expver=<?config.get('expver', '$geff_version')?>
