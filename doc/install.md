
# prepare before run LightTable

## install

  + boot :  https://github.com/boot-clj/boot#install

## download LightTable and sys plugin package (all in one)

  [address]: http://7xnuts.com1.z0.glb.clouddn.com/lighTable.zip

## test data

  + stock history data  : put in the path (csv format) ,see local_data.edn
  + strategy : see example in test/

## check path and config

  if data in different path , need modify config file (??.edn)

# run LightTable

  write a strategy, save as .sys file, or use example file etc. swbr.sys


# build sys file

  select build from project menu, then see the directory of sys file.
  you will find a new sub-directory(output), which have the code generate from sys file.
  boot will compile the output code and lib automatly

# start daemon

  The jar which boot build, could be run as daemon. Please read daemon.md later (still not write)

# continue update and fix bug


