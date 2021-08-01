module ChalkStyles
(
  Style
, black
, red
, green
, yellow
, blue
, magenta
, cyan
, white
, blackBright
, gray
, grey
, redBright
, greenBright
, yellowBright
, blueBright
, magentaBright
, cyanBright
, whiteBright
, bgBlack
, bgRed
, bgGreen
, bgYellow
, bgBlue
, bgMagenta
, bgCyan
, bgWhite
, bgBlackBright
, bgGray
, bgGrey
, bgRedBright
, bgGreenBright
, bgYellowBright
, bgBlueBright
, bgMagentaBright
, bgCyanBright
, bgWhiteBright
, reset
, bold
, dim
, italic
, underline
, inverse
, hidden
, strikethrough
, visible
)

where

newtype Style = Style String

black :: Style
black = Style "black"

red :: Style
red = Style "red"

green :: Style
green = Style "green"

yellow :: Style
yellow = Style "yellow"

blue :: Style
blue = Style "blue"

magenta :: Style
magenta = Style "magenta"

cyan :: Style
cyan = Style "cyan"

white :: Style
white = Style "white"

blackBright :: Style
blackBright = Style "blackBright"

gray :: Style
gray = Style "gray"

grey :: Style
grey = Style "grey"

redBright :: Style
redBright = Style "redBright"

greenBright :: Style
greenBright = Style "greenBright"

yellowBright :: Style
yellowBright = Style "yellowBright"

blueBright :: Style
blueBright = Style "blueBright"

magentaBright :: Style
magentaBright = Style "magentaBright"

cyanBright :: Style
cyanBright = Style "cyanBright"

whiteBright :: Style
whiteBright = Style "whiteBright"

bgBlack :: Style
bgBlack = Style "bgBlack"

bgRed :: Style
bgRed = Style "bgRed"

bgGreen :: Style
bgGreen = Style "bgGreen"

bgYellow :: Style
bgYellow = Style "bgYellow"

bgBlue :: Style
bgBlue = Style "bgBlue"

bgMagenta :: Style
bgMagenta = Style "bgMagenta"

bgCyan :: Style
bgCyan = Style "bgCyan"

bgWhite :: Style
bgWhite = Style "bgWhite"

bgBlackBright :: Style
bgBlackBright = Style "bgBlackBright"

bgGray :: Style
bgGray = Style "bgGray"

bgGrey :: Style
bgGrey = Style "bgGrey"

bgRedBright :: Style
bgRedBright = Style "bgRedBright"

bgGreenBright :: Style
bgGreenBright = Style "bgGreenBright"

bgYellowBright :: Style
bgYellowBright = Style "bgYellowBright"

bgBlueBright :: Style
bgBlueBright = Style "bgBlueBright"

bgMagentaBright :: Style
bgMagentaBright = Style "bgMagentaBright"

bgCyanBright :: Style
bgCyanBright = Style "bgCyanBright"

bgWhiteBright :: Style
bgWhiteBright = Style "bgWhiteBright"

reset :: Style  -- Resets the current color chain.
reset = Style "reset"

bold :: Style  -- Make text bold.
bold = Style "bold"

dim :: Style  -- Emitting only a small amount of light.
dim = Style "dim"

italic :: Style  -- Make text italic. (Not widely supported)
italic = Style "italic"

underline :: Style  -- Make text underline. (Not widely supported)
underline = Style "underline"

inverse :: Style -- Inverse background and foreground colors.
inverse = Style "inverse"

hidden :: Style  -- Prints the text, but makes it invisible.
hidden = Style "hidden"

strikethrough :: Style  -- Puts a horizontal line through the center of the text. (Not widely supported)
strikethrough = Style "strikethrough"

visible :: Style -- Prints the text only when Chalk has a color level > 0. Can be useful for things that are purely cosmetic.
visible = Style "visible"