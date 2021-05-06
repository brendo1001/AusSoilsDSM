rootDir = 'Data'  ## locl path to the data directory - best not to change this at the moment
currentUser = 'LocalPC'  ## will propbably replace this with some login functionality down the track
UseReducedResRasters = T  ## Displays reduce resolution rasters - sppeds u releaflet rendering
rasterResFactor = 5  ## How much to reduce the resolution of display rasters - higher number means lower res
TidyUp = T   ## Delete tempoerary working files generated while do uncertainty bounds calculations
numChunks = 30   ##  controls how much the parallel processing is chunked up. The bigger the number the lesser the memory requirement
DefNumReps = 2  ## Number of sampling reps to do
DefNumFolds = 2  ## number of folds to generate for cross validation in each rep
DefNumCubistRules = 25  ## maximum number of rules Cubist is allowed to generate. More = possibly better model, less = quicker processing
defCubistExtrapThresh = 10  ## Percentage - how for cubist models are allowed to extroplate the predicts past the limits of the input data

helpColour = 'green'  ##  UI paremeter - colour of the help text
