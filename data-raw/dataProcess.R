library(import5eChar) # github.com/oganm/import5eChar
library(purrr)
library(readr)
library(glue)
library(digest)
library(dplyr)
library(XML)
library(ogbox) # github.com/oganm/ogbox
library(wizaRd) # github.com/oganm/wizaRd
library(stringr)
library(memoise)
library(rgeolocate)
library(here)
library(data.table)
library(randomIDs) # add friendlier names. github.com/oganm/randomIDs
library(jsonlite)
library(ipapi)
#usethis::use_data_raw()

set_file_wd = function(){
	command = commandArgs(trailingOnly = FALSE)

	file = gsub('--file=','',command[grepl('--file',command)])
	if(length(file) == 1){
		setwd(dirname(file))
	}
}
set_file_wd()

setwd(here())

# memoisation for quick access
# fc <- cache_filesystem("data-raw/memoiseCache")
# memoImportChar = memoise(importCharacter, cache = fc)

if(file.exists('memoImportChar.rds')){
	memoImportChar = readRDS(here('memoImportChar.rds'))
} else {
	memoImportChar = memoise(importCharacter)
	saveRDS(memoImportChar,'memoImportChar.rds')
}

# get all char files saved everywhere. Yes I made a mess that I refused to fix...
charFiles = c(list.files('/srv/shiny-server/printSheetApp/chars/',full.names = TRUE),
			  list.files('/srv/shiny-server/interactiveSheet/chars/',full.names = TRUE),
			  list.files('/srv/shiny-server/chars',full.names = TRUE),
			  list.files('/srv/shiny-server/chars2', full.names = TRUE),
			  list.files('/srv/shiny-server/chars3', full.names = TRUE),
			  list.files('/srv/shiny-server/chars4', full.names = TRUE))



print('reading char files')

fileInfo = file.info(charFiles)
charFiles = charFiles[order(fileInfo$mtime)]
fileInfo = fileInfo[order(fileInfo$mtime),]

charFiles = charFiles[fileInfo$size!=0]
fileInfo = fileInfo[fileInfo$size!=0,]
# use import5eChar to read the all of them
chars = charFiles %>% lapply(function(x){
	memoImportChar(file = x)
})
saveRDS(memoImportChar,'memoImportChar.rds')

# get date information. dates before 2018-04-16 are not reliable
# get user fingerprint and IP
fileData = charFiles %>% basename %>% strsplit('_')

# add file and user info to the characters
print('constructing char table')
chars = lapply(1:length(chars),function(i){
	char = chars[[i]]
	char$date = fileInfo$mtime[i]
	if(length(fileData[[i]]) == 1){
		char$ip = 'NULL'
		char$finger = 'NULL'
		char$hash = fileData[[i]]
	} else{
		char$finger = fileData[[i]][1]
		char$ip = fileData[[i]][2]
		char$hash = fileData[[i]][3]
	}
	char
})

# setting the names to character name and class. this won't be exposed to others
names(chars) = chars %>% map_chr(function(x){
	paste(x$Name,x$ClassField)
})

# create the table. it initially creates the table because that's what my original pipeline did... later I will convert the
# relevant bits into a list, making this a little silly.
charTable = chars %>% map(function(x){
	hede <<- x
	if((class(x$classInfo) == 'matrix' && nrow(x$classInfo) == 0) ||
	   (class(x$classInfo) == 'matrix' && nrow(x$classInfo) == 1 && x$classInfo[,'Level']=='0')){
		x$classInfo = NULL
	}

	data.frame(ip = x$ip,
			   finger = x$finger,
			   hash = x$hash,
			   name = x$Name,
			   race = x$Race,
			   background = x$Background,
			   date = x$date,
			   class = paste(trimws(x$classInfo[,1]),trimws(x$classInfo[,3]),collapse='|'),
			   justClass =  trimws(x$classInfo[,'Class']) %>% paste(collapse ='|'),
			   subclass = trimws(x$classInfo[,'Archetype']) %>% paste(collapse ='|'),
			   classFreeText = x$ClassField,
			   level = x$classInfo[,'Level'] %>% as.integer() %>% sum,
			   feats = x$feats[x$feats !=''] %>% paste(collapse = '|'),
			   HP = x$currentHealth,
			   AC = AC(x),
			   Str = x$abilityScores['Str'],
			   Dex = x$abilityScores['Dex'],
			   Con = x$abilityScores['Con'],
			   Int = x$abilityScores['Int'],
			   Wis = x$abilityScores['Wis'],
			   Cha = x$abilityScores['Cha'],
			   alignment = x$Alignment,
			   skills = x$skillProf %>% which %>% names %>% paste(collapse = '|'),
			   weapons = x$weapons %>% map_chr('name') %>% gsub("\\|","",.)  %>% paste(collapse = '|'),
			   spells = glue('{x$spells$name %>% gsub("\\\\*|\\\\|","",.)}*{x$spells$level}') %>% glue_collapse('|') %>% {if(length(.)!=1){return('')}else{return(.)}},
			   # day = x$date %>%  format('%m %d %Y'),
			   castingStat = names(x$abilityMods[x$castingStatCode+1]),
			   choices = paste(gsub('\\||/|\\*','',names(x$classChoices)),
			   				sapply(lapply(x$classChoices,gsub,pattern = '\\||/|\\*', replacement = ''),
			   					   paste,collapse = '*'),
			   				sep = "/",collapse = '|'),
			   stringsAsFactors = FALSE)
}) %>% do.call(rbind,.)

# get rid of characters who start with the character generator but continue to level up by hand (unpaid users)
freeTextLevel = charTable$classFreeText %>% str_extract_all('[0-9]+') %>% lapply(as.integer) %>% sapply(sum)

charTable %<>% filter(!(level == 1 & freeTextLevel !=1)) %>%
	filter(class!='')
charTable %<>% select(-classFreeText)

# remove multiple occurances of the same file
charTable %<>% arrange(desc(date)) %>%  filter(!duplicated(hash))


if(file.exists('memoIPgeolocate.rds')){
	memoIPgeolocate = readRDS(here('memoIPgeolocate.rds'))
} else {
	memoIPgeolocate = memoise(ipapi::geolocate)
	saveRDS(memoIPgeolocate,'memoIPgeolocate.rds')
}




ipLocations = charTable$ip %>%
	lapply(memoIPgeolocate,.progress = FALSE) %>%
	rbindlist(fill = TRUE)

saveRDS(memoIPgeolocate,here('memoIPgeolocate.rds'))

charTable$country = ipLocations$country
charTable$countryCode = ipLocations$countryCode
# some experimentation with user location.
# file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
# results <- maxmind(charTable$ip, file, c("continent_name", "country_code", "country_name"))



# post processing -----
# the way races are encoded in the app is a little silly. sub-races are
# not recorded separately. essentially race information is lost other
# than a text field after it's effects are applied during creation.
# The text field is also not too consistent. For instance if you are a
# variant half elf it'll simply say "Variant" but if you are a variant human
# it'll only say human
# here, I define regex that matches races.
# kind of an overkill as only few races actually required special care
races = c(Aarakocra = '(Aarakocra)|(Birdfolk)',
		  Aasimar = 'Aasimar',
		  Bugbear= 'Bugbear',
		  Dragonborn = '(Dragonborn)|(Chromatic)|(Metallic)|(Gem)',
		  Dwarf = 'Dwarf|Warding',
		  Elf = '(?<!Half-)Elf|Drow',
		  Firbolg = 'Firbolg',
		  Genasi= 'Genasi',
		  Gith = 'Geth|Githzerai',
		  Gnome = '(Gnome)|(Scribing)',
		  Goblin='^Goblin$',
		  Goliath = 'Goliath',
		  'Half-Elf' = '(^Half-Elf$)|(^Variant$)|(Detection)|(Storm)|(Half-Elf .Wood.)|(Variant Half-Elf)|(Half-Elf Variant)',
		  'Half-Orc' = 'Half-Orc',
		  Halfling = '(Halfling)|(Hospitality)|(Healing)',
		  Hobgoblin = 'Hobgoblin$',
		  Human = '(Human)|(Variant Human)|(Sentinel)|(Making)|(Passage)',
		  Kenku = 'Kenku',
		  Kobold = 'Kobold',
		  Lizardfolk = 'Lizardfolk',
		  Orc = '(?<!Half-)Orc',
		  'Yaun-Ti' = 'Serpentblood|Yuan-Ti',
		  Tabaxi = 'Tabaxi',
		  Tiefling ='Tiefling|Lineage',
		  Triton = 'Triton',
		  Turtle = 'Turtle|Tortle',
		  Vedalken = 'Violetken|Vedalken',
		  Minotaur = 'Minotaur',
		  Centaur = 'Centaur',
		  Loxodon = 'Elephantine|Luxodon|Loxodon',
		  `Simic hybrid` = 'Animal Hybrid|Simic Hybrid',
		  Warforged = 'Warforged|Envoy|Juggernaut|Juggeenaut',
		  Changeling = 'Changeling',
		  Shifter = '(Shifter)|(Hunt)|(Hide)|(Stride)|(Tooth)',
		  Kalashtar = '(Kalashtar)|(Dreamtouched)',
		  Eladrin = 'Eladrin',
		  Leonin = '(Leonine)|(Leonin)',
		  Satyr = 'Satyr',
		  Custom = 'Custom')

align = list(NG = c('ng',
					'n,g',
					"n.g",
					'neatral good',
					"neutra good",
					'"good"',
					'good',
					'neuteral good',
					'neitral good',
					'neutral good',
					'netual good',
					'nuetral goodt',
					'neutral/good',
					'neutral-good',
					'nuetral good',
					'nutral good',
					'n good',
					'\U0001f937 neutral good',
					'neutral goodsskkd',
					'n/g',
					'neutral  good',
					'n/b',
					'nb',
					'neutral bueno',
					'n. good'),
			 CG = c('chaotic good',
			 	   "chaotic good 👍",
			 	   'chatoic good',
			 	   'chaotic goo',
			 	   'chaothic good',
			 	   'caotica buena',
			 	   'chaotic good.',
			 	   'caótico bueno',
			 	   'cb',
			 	   'cg',
			 	   'chacotic good',
			 	   'c/g',
			 	   'good chaotic'),
			 LG = c('lawful good',
			 	   "lawful good boi",
			 	   'l.g.',
			 	   'laewful good',
			 	   'lawful/good',
			 	   'l/g',
			 	   'l-g',
			 	   'lg',
			 	   'lawfull good',
			 	   'lawful goodness',
			 	   'lawfully good',
			 	   'legal bueno',
			 	   'legal good',
			 	   'lb'),
			 NN = c('neutral',
			 	   "true nutral",
			 	   'nn',
			 	   'loyal neutral',
			 	   'neutal',
			 	   'true n',
			 	   "neutral-neutral",
			 	   'neutral neutral',
			 	   'netral',
			 	   'n',
			 	   'true neutral',
			 	   'tn',
			 	   'true-neutral',
			 	   'leal neutro',
			 	   'nuetral',
			 	   'neutral verdadero',
			 	   'neutro',
			 	   'true nuetral'),
			 CN = c('chaotic neutral',
			 	   "chaotic n",
			 	   "chaotic nutral",
			 	   "caotico e neutro",
			 	   "c n",
			 	   'chaotique neutre',
			 	   'neutral caotico',
			 	   'caotic neutral',
			 	   'chaotic-neutral',
			 	   'c/n',
			 	   'caótico neutro',
			 	   'chaotic netural',
			 	   'chaotic',
			 	   'cn',
			 	   'chaotic nuetral',
			 	   'chatoic neutral',
			 	   'chatic neutral',
			 	   'neutral chaotic',
			 	   'chaotic - neutral',
			 	   'chaotic neutrall',
			 	   'caotico neutral',
			 	   'caótico neutral',
			 	   "тру хаотик"),
			 LN = c('lawful neutral',
			 	   "neutral lawful",
			 	   "l n",
			 	   'l.n',
			 	   'lawful nuetral',
			 	   'lawfull neutral',
			 	   'legal neutral',
			 	   'lawful neitral',
			 	   'lawful',
			 	   'lawful/neutral',
			 	   'leal e neutro',
			 	   'lawful - neutral',
			 	   'ln',
			 	   'l/n',
			 	   'lawful neutral (good-ish)'),
			 NE = c('neutral evil','ne','n/e',
			 	   "nuetral evil",
			 	   'neutral malvado',
			 	   'neutral maligno'),
			 LE = c('lawful evil',
			 	   'lawfuo evil',
			 	   'lawful/evil',
			 	   'lawful evik',
			 	   'le',
			 	   'legal malvado',
			 	   'l/e'),
			 CE = c('ce',
			 	   'chaotic evil',
			 	   'caótico malvado',
			 	   'caotico maligno'
			 	   ))

goodEvil = list(`E` = c('NE','LE','CE'),
				`N` = c('LN','CN','NN'),
				`G` = c('NG','LG','CG'))

lawfulChaotic = list(`C` = c('CN','CG','CE'),
					 `N` = c('NG','NE','NN'),
					 `L` = c('LG','LE','LN'))

# lists any alignment text I'm not processing
charTable$alignment  %>% {.[!tolower(trimws(.)) %in% unlist(align)]} %>% table %>% sort %>% names %>% tolower %>% trimws

checkAlignment = function(x,legend){
	x = names(legend)[findInList(tolower(trimws(x)),legend)]
	if(length(x) == 0){
		return('')
	} else{
		return(x)
	}
}


charTable %<>% mutate(processedAlignment = alignment %>% purrr::map_chr(checkAlignment,align),
					  good = processedAlignment %>% purrr::map_chr(checkAlignment,goodEvil) %>%
					  	factor(levels = c('E','N','G')),
					  lawful = processedAlignment %>%
					  	purrr::map_chr(checkAlignment,lawfulChaotic) %>% factor(levels = c('C','N','L')))

charTable %<>% mutate(processedRace = race %>% sapply(function(x){
	out = races %>% sapply(function(y){
		grepl(pattern = y, x,perl = TRUE,ignore.case = TRUE)
	}) %>% which %>% names

	if(length(out) == 0 | length(out)>1){
		out = ''
	}

	return(out)
}))

#  lists any race text I'm not processing
charTable$processedRace[charTable$processedRace == ""] %>% names %>% table %>% sort

# process spells -----
spells = wizaRd::spells

spells = c(spells, list('.' = list(level = as.integer(99))))
class(spells) = 'list'

legitSpells =spells %>% names

trimPunct = function(char){
	gsub('[[:punct:]]+','',char)
}

processedSpells = charTable$spells %>% sapply(function(x){
	if(x==''){
		return('')
	}
	spellNames = x %>% str_split('\\|') %>% {.[[1]]} %>% str_split('\\*') %>% map_chr(1)
	spellLevels =  x %>% str_split('\\|') %>% {.[[1]]} %>% str_split('\\*') %>% map_chr(2)

	distanceMatrix = adist(tolower(spellNames), tolower(legitSpells),costs = list(ins=4, del=4, sub=6), counts = TRUE)

	rownames(distanceMatrix) = spellNames
	colnames(distanceMatrix) = legitSpells

	predictedSpell = distanceMatrix %>% apply(1,which.min) %>% {legitSpells[.]}
	distanceScores =  distanceMatrix %>% apply(1,min)
	predictedSpellLevel = spells[predictedSpell] %>% purrr::map_int('level')

	ins = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'ins'] %>% as.matrix  %>% diag
	del = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'del'] %>% as.matrix %>% diag
	sub = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'sub'] %>% as.matrix %>% diag
	# check if all words of the prediction is in the written spell
	isItIn = predictedSpell %>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
		{sapply(1:length(.),function(i){
			all(sapply(trimPunct(tolower(.[[i]])),grepl,x =trimPunct(tolower(spellNames[i])),fixed = TRUE))
		})}

	# check if all words of the spell is in the prediction
	isTheSpellIn = spellNames%>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
		{sapply(1:length(.),function(i){
			all(sapply(trimPunct(tolower(.[[i]])),grepl,x =trimPunct(tolower(predictedSpell[i])), fixed = TRUE))
		})}

	spellFrame = data.frame(spellNames,predictedSpell,spellLevels,predictedSpellLevel,distanceScores,ins,del,sub,isItIn,isTheSpellIn,stringsAsFactors = FALSE)

	# special cases for some badly matched spells
	if(any(trimws(tolower(spellFrame$spellNames)) == 'arcane hand' & spellFrame$spellLevels==5)){
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'arcane hand' & spellFrame$spellLevels==5,]$predictedSpell = "Bigby's Hand"
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'arcane hand'  & spellFrame$spellLevels==5,]$isItIn = TRUE
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'arcane hand'  & spellFrame$spellLevels==5,]$predictedSpellLevel = 5

	}

	if(any(trimws(tolower(spellFrame$spellNames)) == 'acid arrow' & spellFrame$spellLevels==2)){
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'acid arrow' & spellFrame$spellLevels==2,]$predictedSpell = "Melf's Acid Arrow"
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'acid arrow' & spellFrame$spellLevels==2,]$isItIn = TRUE
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'acid arrow' & spellFrame$spellLevels==2,]$predictedSpellLevel = 2

	}

	if(any(trimws(tolower(spellFrame$spellNames)) == 'hideaous laughter' & spellFrame$spellLevels==1)){
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'hideaous laughter' & spellFrame$spellLevels==1,]$predictedSpell = "Tasha's Hideous Laughter"
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'hideaous laughter' & spellFrame$spellLevels==1,]$isItIn = TRUE
		spellFrame[trimws(tolower(spellFrame$spellNames)) == 'hideaous laughter' & spellFrame$spellLevels==1,]$predictedSpellLevel = 1

	}

	# remove matches that don't satisfy the similarity criteria
	spellFrame$predictedSpell[!(as.integer(spellFrame$spellLevels)==spellFrame$predictedSpellLevel &(spellFrame$isTheSpellIn | spellFrame$isItIn | (spellFrame$sub < 10 & spellFrame$del < 10 & spellFrame$ins < 10)))] = ''
	spellFrame$predictedSpellLevel[!(as.integer(spellLevels)==predictedSpellLevel &(isTheSpellIn | isItIn | (sub < 10 & del < 10 & ins < 10)))] = ''
	# spellFrame %<>% filter(as.integer(spellLevels)==predictedSpellLevel &(isTheSpellIn | isItIn | (sub < 5 & del < 5 & ins < 5)))

	paste0(spellFrame$predictedSpell,'*',spellFrame$predictedSpellLevel,collapse ='|')
})

charTable$processedSpells = processedSpells


# manual checking of randomly selected data. select random spell/processed spell pairs. manually examine them to make sure
# they are allright and estimate accuracy.
withSpells = which(charTable$spells !='')

withSpells %>% lapply(function(i){
	rawSpells = charTable$spells[i] %>% strsplit('\\|') %>% {.[[1]]}
	pSpells =  charTable$processedSpells[i] %>% strsplit('\\|') %>% {.[[1]]}
	seq_along(rawSpells) %>% sapply(function(j){
		c(i,rawSpells[j],pSpells[j])
	}) %>% t
}) %>% do.call(rbind,.) ->  spellProcessedPairs

# 200 random pairs
# spellProcessedPairs[spellProcessedPairs[,3] !='*' & spellProcessedPairs[,2] != spellProcessedPairs[,3],][sample(1:nrow(spellProcessedPairs[spellProcessedPairs[,3] !='*' & spellProcessedPairs[,2] != spellProcessedPairs[,3],]),200),]

# all spells that couldn't be matched
# spellProcessedPairs[spellProcessedPairs[,3] =='*',-3] %>% View

spellCount = spellProcessedPairs %>% nrow
standardSpellCount = nrow(spellProcessedPairs[spellProcessedPairs[,3] !='*' & spellProcessedPairs[,2] == spellProcessedPairs[,3],])
nonStandardSpellCount = nrow(spellProcessedPairs[spellProcessedPairs[,3] !='*' & spellProcessedPairs[,2] != spellProcessedPairs[,3],])
mismatchCount = spellProcessedPairs[spellProcessedPairs[,3] =='*',-3] %>% nrow

nonStandardSpellCount/spellCount * 100
mismatchCount/spellCount * 100
standardSpellCount/spellCount * 100

# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[43]}
# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[70]}
# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[88]}

# download.file('https://www.dropbox.com/s/4f7zdx09nkfa9as/Core.xml?dl=1',destfile = 'Core.xml')
# allRules = xmlParse('Core.xml') %>% xmlToList()
# fightClubItems = allRules[names(allRules) == 'item']
# saveRDS(fightClubItems,'fightClubItems.rds')

# fightClubItems =  readRDS('fightClubItems.rds')
# names(fightClubItems) = allRules %>% map('name') %>% as.character
#
# fightClubItems %>% map_chr('type') %>% {. %in% 'M'} %>% {fightClubItems[.]} %>% map_chr('name')
# fightClubItems %>% map_chr('type') %>% {. %in% 'R'} %>% {fightClubItems[.]} %>% map_chr('name')

legitWeapons = c(# fightClubItems %>% map_chr('type') %>% {. %in% 'M'} %>% {fightClubItems[.]} %>% map_chr('name'),
	# fightClubItems %>% map_chr('type') %>% {. %in% 'R'} %>% {fightClubItems[.]} %>% map_chr('name'),
	'Crossbow, Light', 'Dart', 'Shortbow', 'Sling',
	'Blowgun', 'Crossbow, hand', 'Crossbow, Heavy', 'Longbow', 'Net',
	'Club','Dagger','Greatclub','Handaxe','Javelin','Light hammer','Mace','Quarterstaff','Sickle','Spear','Unarmed Strike',
	'Battleaxe','Flail','Glaive','Greataxe','Greatsword','Halberd','Lance','Longsword','Maul','Morningstar','Pike','Rapier','Scimitar','Shortsword','Trident','War pick','Warhammer','Whip')

processedWeapons = charTable$weapons %>% sapply(function(x){
	if(x==''){
		return('')
	}
	weaponNames = x %>% str_split('\\|') %>% {.[[1]]}

	distanceMatrix = adist(tolower(weaponNames), tolower(legitWeapons),costs = list(ins=2, del=2, sub=3), counts = TRUE)

	rownames(distanceMatrix) = weaponNames
	colnames(distanceMatrix) = legitWeapons

	predictedWeapon = distanceMatrix %>% apply(1,which.min) %>% {legitWeapons[.]}
	distanceScores =  distanceMatrix %>% apply(1,min)

	ins = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'ins'] %>% as.matrix  %>% diag
	del = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'del'] %>% as.matrix %>% diag
	sub = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'sub'] %>% as.matrix %>% diag
	isItIn = predictedWeapon %>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
	{sapply(1:length(.),function(i){
		all(sapply(trimPunct(.[[i]]),grepl,x =trimPunct(weaponNames[i]),ignore.case=TRUE))
	})}

	isTheWeaponIn = weaponNames%>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
		{sapply(1:length(.),function(i){
			all(sapply(trimPunct(tolower(.[[i]])),grepl,x =trimPunct(tolower(predictedWeapon[i])), fixed = TRUE))
		})}

	weaponFrame = data.frame(weaponNames,predictedWeapon,distanceScores,ins,del,sub,isItIn,isTheWeaponIn,stringsAsFactors = FALSE)


	if(any(trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'hand crossbow')){
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'hand crossbow',]$predictedWeapon = 'Crossbow, hand'
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'hand crossbow',]$isItIn = TRUE

	}

	if(any(trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'heavy crossbow')){
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'heavy crossbow',]$predictedWeapon = 'Crossbow, Heavy'
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == 'heavy crossbow',]$isItIn = TRUE
	}

	if(any(trimPunct(trimws(tolower(weaponFrame$weaponNames))) == '')){
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == '',]$predictedWeapon = ''
		weaponFrame[trimPunct(trimws(tolower(weaponFrame$weaponNames))) == '',]$isItIn = TRUE
	}

	weaponFrame$predictedWeapon[!(weaponFrame$isTheWeaponIn | weaponFrame$isItIn | (weaponFrame$sub < 2 & weaponFrame$del<2 & weaponFrame$ins<2))] = ''


	# weaponFrame %<>% filter(isItIn|  (sub < 2 & del < 2 & ins < 2))

	paste0(weaponFrame$predictedWeapon,collapse ='|')
})

charTable$processedWeapons = processedWeapons

# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$weapons[i],charTable$processedWeapons[i])}) %>% {.>20} %>% {charTable$weapons[.]} %>% {.[10]}


withWeapons = which(charTable$weapons !='')
withWeapons %>% lapply(function(i){
	rawWeapons = charTable$weapons[i] %>% stringr::str_split('\\|') %>% {.[[1]]}
	pWeapons =  charTable$processedWeapons[i] %>% stringr::str_split('\\|') %>% {.[[1]]}
	seq_along(rawWeapons) %>% sapply(function(j){
		c(i,rawWeapons[j],pWeapons[j])
	}) %>% t
}) %>% do.call(rbind,.) ->  weaponProcessedPairs

# weaponProcessedPairs[weaponProcessedPairs[,2] != weaponProcessedPairs[,3] & weaponProcessedPairs[,3]!='',] %>% {.[sample(nrow(.),200),]} %>% View


weaponCount = weaponProcessedPairs %>% nrow
standardWeaponCount = nrow(weaponProcessedPairs[weaponProcessedPairs[,2] == weaponProcessedPairs[,3],])
nonStandardWeaponCount = nrow(weaponProcessedPairs[weaponProcessedPairs[,2] != weaponProcessedPairs[,3] & weaponProcessedPairs[,3] !='',])
mismatchCount = weaponProcessedPairs[weaponProcessedPairs[,3] =='',] %>% nrow

nonStandardWeaponCount/weaponCount * 100
mismatchCount/weaponCount * 100
standardWeaponCount/weaponCount * 100

# user id ------
# userID = c()
# pb = txtProgressBar(min = 0, max = nrow(charTable), initial = 0)
#
# for(i in 1:nrow(charTable)){
#     setTxtProgressBar(pb,i)
#     for (id in unique(userID)){
#         userChars = charTable[which(userID == id),]
#         ip = charTable$ip[i] %>% {if(is.na(.) || . =='NULL' || .==''){return("NANA")}else{.}}
#         finger = charTable$finger[i] %>% {if(is.na(.) || . =='NULL' ||. == ''){return("NANA")}else{.}}
#         hash = charTable$hash[i] %>% {if(is.na(.) || . =='NULL' || . == ''){return("NANA")}else{.}}
#
#         ipInUser = ip %in% userChars$ip
#         fingerInUser = finger %in% userChars$finger
#         hashInUser = hash %in% userChars$hash
#         if(ipInUser | fingerInUser | hashInUser){
#
#             userID = c(userID,id)
#             break
#         }
#
#     }
#
#     if(length(userID)!=i){
#         userID = c(userID, max(c(userID,0))+1)
#     }
# }
#
# charTable$userID = userID
#
#
# userID = c()
# pb = txtProgressBar(min = 0, max = nrow(charTable), initial = 0)
#
# for(i in 1:nrow(charTable)){
#     setTxtProgressBar(pb,i)
#     for (id in unique(userID)){
#         userChars = charTable[which(userID == id),]
#         ip = charTable$ip[i] %>% {if(is.na(.) || . =='NULL' || .==''){return("NANA")}else{.}}
#         finger = charTable$finger[i] %>% {if(is.na(.) || . =='NULL' ||. == ''){return("NANA")}else{.}}
#         hash = charTable$hash[i] %>% {if(is.na(.) || . =='NULL' || . == ''){return("NANA")}else{.}}
#
#         ipInUser = ip %in% userChars$ip
#         fingerInUser = finger %in% userChars$finger
#         hashInUser = hash %in% userChars$hash
#         if(fingerInUser | hashInUser){
#
#             userID = c(userID,id)
#             break
#         }
#
#     }
#
#     if(length(userID)!=i){
#         userID = c(userID, max(c(userID,0))+1)
#     }
# }
#
# charTable$userIDNoIP = userID

# group levels at common feat acquisition points. sorry fighters and rogues

charTable %<>% mutate(levelGroup = cut(level,
									   breaks = c(0,3,7,11,15,18,20),
									   labels  = c('1-3','4-7','8-11','12-15','16-18','19-20')))


# remove personal info -----------

shortestDigest = function(vector){
	digested = vector(mode = 'character',length = length(vector))
	digested[vector!='']  = vector[vector!=''] %>% map_chr(digest,'sha1')
	uniqueDigested =  digested[digested!=''] %>% unique
	collusionLimit = 1:40 %>% sapply(function(i){
		substr(uniqueDigested,40-i,40)%>% unique %>% length
	}) %>% which.max %>% {.+1}
	digested %<>%  substr(40-collusionLimit,40)
	return(digested)
}


charTable$name %<>% shortestDigest
charTable$ip %<>% shortestDigest
charTable$finger %<>% shortestDigest
# charTable %<>% select(-hash)
# unsecureFields = c('ip','finger','hash')
# charTable = charTable[!names(charTable) %in% unsecureFields]

# add friendly names ensure old names remain the same
# the hashes will actually change but their order of introduction shouldn't
set.seed(1)
uniqueNames = charTable %>% arrange(date) %$% name %>% unique
randomAlias = random_names(length(uniqueNames))
names(randomAlias) = uniqueNames
charTable %<>% mutate(alias = randomAlias[name])


dnd_chars_all = charTable
write_tsv(dnd_chars_all,path = here('data-raw/dnd_chars_all.tsv'))
usethis::use_data(dnd_chars_all,overwrite = TRUE)


# get unique table ----------------
getUniqueTable = function(charTable){
	# remove obvious duplicates. same name and class assumed to be dups
	# race is not considered in case same person is experimenting with different
	# races
	charTable %<>% filter(name !='')

	uniqueTable = charTable %>% arrange(desc(level)) %>%
		filter(!duplicated(paste(name,justClass))) %>%
		filter(!level > 20)

	# detect non unique characters that multiclassed
	multiClassed = uniqueTable %>% filter(grepl('\\|',justClass))
	singleClassed = uniqueTable %>% filter(!grepl('\\|',justClass))

	multiClassDuplicates = multiClassed$name %>% duplicated %>% which

	# this is somewhat of a heuristic since it only looks at total level and classes chosen
	# but as both name and class combination is the same its probably some guy experimenting
	# with different character ideas.
	multiClassDuplicates %>% sapply(function(x){

		thedup = multiClassed[x,]
		matches = multiClassed[-x,] %>% filter(name == thedup$name)

		higherLevel = thedup$level < matches$level
		dupClass = strsplit(thedup$justClass,'\\|')[[1]]
		matchClass = strsplit(matches$justClass,'\\|')

		matchClass %>% sapply(function(y){
			all(dupClass %in% y)
		}) -> classMatches

		any(classMatches & higherLevel)

	}) -> isMultiClassDuplicate
	if(length(multiClassDuplicates[isMultiClassDuplicate])>0){
		multiClassed = multiClassed[-multiClassDuplicates[isMultiClassDuplicate],]
	}

	matchingNames = multiClassed$name[multiClassed$name %in% singleClassed$name] %>%
		unique

	singleCharDuplicates = which(singleClassed$name %in% matchingNames)

	singleCharDuplicates %>% sapply(function(x){
		char = singleClassed[x,]
		# print(char[['name']])
        multiChar = multiClassed %>%
        	filter(name %in% char[['name']] & grepl(char[['justClass']],justClass))
        if(nrow(multiChar) == 0){
        	return (FALSE)
        }

        isHigher = any(multiChar$level > char[['level']])
        if (nrow(multiChar)>1){
        	# warning("multiple matches")
        }
        return(isHigher)
	}) -> isDuplicate
	if(length(singleCharDuplicates[isDuplicate])>0){
		singleClassed = singleClassed[-singleCharDuplicates[isDuplicate],]
	}

	uniqueTable = rbind(singleClassed,multiClassed)

	return(list(uniqueTable = uniqueTable,
				singleClassed = singleClassed,
				multiClassed = multiClassed))
}


# dnd_chars_all = read_tsv(here("data-raw/dnd_chars_all.tsv"),na = 'NA') # redundant


list[dnd_chars_unique,dnd_chars_singleclass,dnd_chars_multiclass] = getUniqueTable(dnd_chars_all)

write_tsv(dnd_chars_unique,path = here('data-raw/dnd_chars_unique.tsv'))

usethis::use_data(dnd_chars_unique,overwrite = TRUE)
usethis::use_data(dnd_chars_singleclass,overwrite = TRUE)
usethis::use_data(dnd_chars_multiclass,overwrite = TRUE)

table2list = function(charTable){
	seq_len(nrow(charTable)) %>% lapply(function(i){
		char = charTable[i,]

		list(ip = char$ip,
			 finger = char$finger,
			 name = list(
			 	hash = char$name,
			 	alias = char$alias),
			 race = list(
			 	race = char$race,
			 	processedRace = char$processedRace
			 ),
			 background = char$background,
			 date = char$date,
			 class = seq_len(strsplit(char$class,'\\|') %>% {.[[1]]} %>% length) %>%
			 	lapply(function(j){
			 		list(
			 			class = char$justClass %>% strsplit('\\|') %>% {out = .[[1]][j];if(is.na(out)){return('')}else{return(out)}},
			 			subclass = char$subclass %>% strsplit('\\|') %>% {out = .[[1]][j];if(is.na(out)){return('')}else{return(out)}},
			 			level = char$class %>% strsplit('\\|') %>% {.[[1]][j]} %>% str_extract('[0-9]+') %>% as.integer()
			 		)
			 	}) %>% {names(.) = strsplit(char$justClass,'\\|') %>% {.[[1]]};.},
			 level = char$level,
			 levelGroup = char$levelGroup,
			 feats = char$feats %>% strsplit('\\|') %>% {.[[1]]},
			 HP = char$HP,
			 AC = char$AC,
			 attributes = list(Str = char$Str,
			 				  Dex = char$Dex,
			 				  Con = char$Con,
			 				  Int = char$Int,
			 				  Wis = char$Wis,
			 				  Cha = char$Cha),
			 alignment = list(
			 	alignment = char$alignment,
			 	processedAlignment = char$processedAlignment,
			 	lawful = char$lawful,
			 	good = char$good
			 ),
			 skills = char$skills %>% strsplit('\\|') %>% {.[[1]]},
			 weapons = seq_along(strsplit(char$weapons,'\\|') %>% {.[[1]]}) %>% lapply(function(j){
			 	list(
			 		weapon = char$weapons %>% strsplit('\\|') %>% {.[[1]][j]},
			 		processedWeapon = char$processedWeapons %>% strsplit('\\|') %>% {.[[1]][j]}
			 	)
			 }) %>% {names(.) = strsplit(char$weapons,'\\|') %>% {.[[1]]};.},
			 spells = seq_along(strsplit(char$spells,'\\|') %>% {.[[1]]}) %>% lapply(function(j){
			 	list(
			 		spell = char$spells %>% strsplit('\\|') %>% {.[[1]][j]} %>% strsplit('\\*') %>% {.[[1]][1]},
			 		level = char$spells %>% strsplit('\\|') %>% {.[[1]][j]} %>% strsplit('\\*') %>% {.[[1]][2]},
			 		processedSpell = char$processedSpells %>% strsplit('\\|') %>% {.[[1]][j]} %>% strsplit('\\*') %>% {.[[1]][1]}
			 	)
			 }) %>% {names(.) = strsplit(char$spells,'\\|') %>% {.[[1]]};.},
			 castingStat = char$castingStat,
			 choices = seq_along(strsplit(char$choices,'\\|') %>% {.[[1]]}) %>% lapply(function(j){
			 	char$choices %>% strsplit('\\|') %>% {.[[1]][j]} %>% strsplit('/') %>% {.[[1]][2]} %>% strsplit('\\*') %>% {.[[1]]}
			 }) %>% {names(.) = char$choices %>% strsplit('\\|') %>% unlist %>% strsplit('/') %>% map_chr(1);.},
			 location = list(country = char$country %>% as.character,
			 				countryCode = char$countryCode %>% as.character),
			 hash = char$hash
			 )
	}) %>% {names(.) = paste(charTable$alias,charTable$class);.}
}

dnd_chars_unique_list = table2list(dnd_chars_unique)
dnd_chars_singleclass_list = table2list(dnd_chars_singleclass)
dnd_chars_multiclass_list = table2list(dnd_chars_multiclass)
dnd_chars_all_list = table2list(dnd_chars_all)



usethis::use_data(dnd_chars_unique_list,overwrite = TRUE)
usethis::use_data(dnd_chars_singleclass_list,overwrite = TRUE)
usethis::use_data(dnd_chars_multiclass_list,overwrite = TRUE)
usethis::use_data(dnd_chars_all_list,overwrite = TRUE)

dnd_chars_unique_list %>% jsonlite::toJSON(pretty = TRUE) %>% writeLines(here('data-raw/dnd_chars_unique.json'))
dnd_chars_all_list %>% jsonlite::toJSON(pretty = TRUE) %>% writeLines(here('data-raw/dnd_chars_all.json'))


