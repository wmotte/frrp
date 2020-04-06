# aim: add journal information to file "journals.csv"
#
# h.j.lamberink@umcutrecht.nl
# date: march 23, 2018
################################################################################


library( stringr )
library( plyr );library( dplyr )
library( ggplot2 )

# the file journals.csv contains journal names in the form of ISO_ABBREV
# the files journalIF.csv contain journal names in the form of TITLE
# the file jinfo has both forms ISO_ABBREV and TITLE


# read file with 326k included items
journals <- read.csv( "journals.csv", stringsAsFactors = F )
names(journals)[2] <- "ISO_ABBREV"

# read Thomson-Reuters journal information sheet
jinfoSCIE <- read.csv2( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/JCR_SCIE_2016.csv", stringsAsFactors = F )
jinfoSSCI <- read.csv2( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/JCR_SSCI_2016.csv", stringsAsFactors = F )
jinfo <- rbind(jinfoSCIE, jinfoSSCI)
jinfo <- jinfo[, c( "Title20", "TITLE", "ISO_ABBREV", "ISSUES.YEAR", "COUNTRY.REGION", "PUBLISHER_NAME", "X1ST_YR_PUB" ) ]

# read journal category file
jcat <- read.csv2( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/journal_categories.csv", stringsAsFactors = F )

# other abberations in jinfo file
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Intern. Med.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch intern med"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Psychiatry", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch gen psychiat"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Psychiatry", ] # two versions
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch gen psychiatry"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Ophthalmol.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch ophthalmol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Pediatr.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch pediatr adolesc med"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Surg.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch surg"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Dermatol.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch dermatol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Neurol.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch neurol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Bone Joint J.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "j bone joint surg br"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "J. Acad. Nutr. Diet.", ] # changed name in 2012
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "j am diet assoc"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "J. Trauma Acute Care Surg.", ] # changed name in 2012
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "j trauma"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Contemp. Clin. Trials", ] # changed name in 2005
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "control clin trials"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "BJU Int.", ] # changed name in 2005
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "br j urol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "JAMA Otolaryngol-Head Neck Surg.", ] # changed name in 2005
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "arch otolaryngol head neck surg"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Region. Anesth. Pain Med.", ] # changed name in 1998
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "reg anesth"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Heart", ] # changed name in 1996
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "br heart j"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Scand. J. Urol.", ] # changed name in 2013
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "scand j urol nephrol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "J. Hypertens.", ]
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "j hypertens suppl"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Scand. J. Gastroenterol.", ]
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "scand j gastroenterol suppl"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "RHEUMATOLOGY", ] # changed name in 1999
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "br j rheumatol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Obesity", ] # changed name in 2006
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "obes res"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Eur. J. Prev. Cardiol.", ] # changed name in 2006
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "eur j cardiovasc prev rehabil"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Eur. J. Anaesth.", ] # two notations
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "eur j anaesthesiol"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "br med j"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "bmj"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "br med j (clin res ed)"
jinfo[ length(jinfo[,1]) + 1,  ] <- jinfo[ jinfo$ISO_ABBREV == "Obesity", ] # two notations
jinfo[ length(jinfo[,1]), "ISO_ABBREV" ] <- "obesity silver spring"



jinfo[ jinfo$ISO_ABBREV == "BMJ-British Medical Journal", "ISO_ABBREV" ] <- "BMJ"
jinfo[ jinfo$ISO_ABBREV == "JAMA-J. Am. Med. Assoc.", "ISO_ABBREV" ] <- "jama"
jinfo[ jinfo$ISO_ABBREV == "JAIDS-JOURNAL OF ACQUIRED IMMUNE DEFICIENCY SYNDROMES", "ISO_ABBREV" ] <- "jaids"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Psychiat.", "ISO_ABBREV" ] <- "am j psychiatry"
jinfo[ jinfo$ISO_ABBREV == "Can. J. Anesth.", "ISO_ABBREV" ] <- "can j anaesth" # wrongly indicated by PUBMED
jinfo[ jinfo$ISO_ABBREV == "Eur. Resp. J.", "ISO_ABBREV" ] <- "eur respir j" # wrongly indicated by PUBMED
jinfo[ jinfo$ISO_ABBREV == "J. Periodont.", "ISO_ABBREV" ] <- "j periodontol" # wrongly indicated by PUBMED
jinfo[ jinfo$ISO_ABBREV == "J. Bone Joint Surg.-Am. Vol.", "ISO_ABBREV" ] <- "j bone joint surg am"
jinfo[ jinfo$ISO_ABBREV == "Metab.-Clin. Exp.", "ISO_ABBREV" ] <- "metab clin exp"
jinfo[ jinfo$ISO_ABBREV == "JAIDS", "ISO_ABBREV" ] <- "j acquir immune defic syndr"
jinfo[ jinfo$ISO_ABBREV == "Pediatr.  Anesth.", "ISO_ABBREV" ] <- "paediatr anaesth"
jinfo[ jinfo$ISO_ABBREV == "Clin. Endocrinol.", "ISO_ABBREV" ] <- "clin endocrinol oxf"
jinfo[ jinfo$ISO_ABBREV == "Mennopause-J. N. Am. Menopause Soc.", "ISO_ABBREV" ] <- "menopause"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Gynecol. Obstet.", "ISO_ABBREV" ] <- "int j gynaecol obstet"
jinfo[ jinfo$ISO_ABBREV == "Region. Anesth. Pain Med.", "ISO_ABBREV" ] <- "reg anesth pain med"
jinfo[ jinfo$ISO_ABBREV == "JNCI-J. Natl. Cancer Inst.", "ISO_ABBREV" ] <- "j natl cancer inst"
jinfo[ jinfo$ISO_ABBREV == "J. Obstet. Gynaecol.", "ISO_ABBREV" ] <- "j obstet gynaecol"
jinfo[ jinfo$ISO_ABBREV == "Osteoporosis Int.", "ISO_ABBREV" ] <- "osteoporos int"
jinfo[ jinfo$ISO_ABBREV == "J. Am. Acad. Child Adolesc. Psychiatr.", "ISO_ABBREV" ] <- "j am acad child adolesc psychiatry"
jinfo[ jinfo$ISO_ABBREV == "J. Arthroplast.", "ISO_ABBREV" ] <- "j arthroplasty"
jinfo[ jinfo$ISO_ABBREV == "J. Psychopharmacol.", "ISO_ABBREV" ] <- "j psychopharmacol oxford"
jinfo[ jinfo$ISO_ABBREV == "Clin. Orthop. Rel. Res.", "ISO_ABBREV" ] <- "clin orthop relat res"
jinfo[ jinfo$ISO_ABBREV == "PACE-Pacing Clin. Electrophysiol.", "ISO_ABBREV" ] <- "pacing clin electrophysiol"
jinfo[ jinfo$ISO_ABBREV == "Alcoholism (NY)", "ISO_ABBREV" ] <- "alcohol clin exp res"
jinfo[ jinfo$ISO_ABBREV == "World J.Surg.", "ISO_ABBREV" ] <- "world j surg"
jinfo[ jinfo$ISO_ABBREV == "J. Parenter. Enter. Nutr.", "ISO_ABBREV" ] <- "jpen j parenter enteral nutr"
jinfo[ jinfo$ISO_ABBREV == "Clin. Drug Invest.", "ISO_ABBREV" ] <- "clin drug investig"
jinfo[ jinfo$ISO_ABBREV == "RHEUMATOLOGY", "ISO_ABBREV" ] <- "rheumatology oxford"
jinfo[ jinfo$ISO_ABBREV == "Arch. Dis. Child.-Fetal Neonatal Ed.", "ISO_ABBREV" ] <- "arch dis child fetal neonatal ed"
jinfo[ jinfo$ISO_ABBREV == "Trans. Roy. Soc. Trop. Med. Hyg.", "ISO_ABBREV" ] <- "trans r soc trop med hyg"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Orthod. Dentofac. Orthop.", "ISO_ABBREV" ] <- "am j orthod dentofacial orthop"
jinfo[ jinfo$ISO_ABBREV == "Eye", "ISO_ABBREV" ] <- "eye lond"
jinfo[ jinfo$ISO_ABBREV == "J. Sports Med. Phys. Fit.", "ISO_ABBREV" ] <- "j sports med phys fitness"
jinfo[ jinfo$ISO_ABBREV == "Hum. Psychopharmacol.-Clin. Exp.", "ISO_ABBREV" ] <- "hum psychopharmacol"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Obes.", "ISO_ABBREV" ] <- "int j obes lond"
jinfo[ jinfo$ISO_ABBREV == "SAMJ S. Afr. Med. J.", "ISO_ABBREV" ] <- "s afr med j"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Clin. Oncol.-Cancer Clin. Trials", "ISO_ABBREV" ] <- "am j clin oncol"
jinfo[ jinfo$ISO_ABBREV == "Antivir. Ther.", "ISO_ABBREV" ] <- "antivir ther lond"
jinfo[ jinfo$ISO_ABBREV == "Clin. Oral Implant. Res.", "ISO_ABBREV" ] <- "clin oral implants res"
jinfo[ jinfo$ISO_ABBREV == "J. Matern.-Fetal Neonatal Med.", "ISO_ABBREV" ] <- "j matern fetal neonatal med"
jinfo[ jinfo$ISO_ABBREV == "JACC-Cardiovasc. Interv.", "ISO_ABBREV" ] <- "jacc cardiovasc interv"
jinfo[ jinfo$ISO_ABBREV == "J. Gerontol. Ser. A-Biol. Sci. Med. Sci.", "ISO_ABBREV" ] <- "j gerontol a biol sci med sci"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Geriatr. Psychiatr.", "ISO_ABBREV" ] <- "am j geriatr psychiatry"
jinfo[ jinfo$ISO_ABBREV == "J. Psychosomat. Res.", "ISO_ABBREV" ] <- "j psychosom res"
jinfo[ jinfo$ISO_ABBREV == "J. Manip. Physiol. Ther.", "ISO_ABBREV" ] <- "j manipulative physiol ther"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Geriatr. Psychiatr.", "ISO_ABBREV" ] <- "int j geriatr psychiatry"
jinfo[ jinfo$ISO_ABBREV == "J. Dermatol. Treat.", "ISO_ABBREV" ] <- "j dermatolog treat"
jinfo[ jinfo$ISO_ABBREV == "Nutr. Metab. Carbiovasc. Dis.", "ISO_ABBREV" ] <- "nutr metab cardiovasc dis"
jinfo[ jinfo$ISO_ABBREV == "Eur. Arch. Oto-Rhino-Laryn.", "ISO_ABBREV" ] <- "eur arch otorhinolaryngol"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Clin. Pharmacol. Ther. Toxicol.", "ISO_ABBREV" ] <- "int j clin pharmacol ther"
jinfo[ jinfo$ISO_ABBREV == "Acta Derm.-Venereol.", "ISO_ABBREV" ] <- "acta derm venereol"
jinfo[ jinfo$ISO_ABBREV == "Psycho-Oncol.", "ISO_ABBREV" ] <- "psychooncology"
jinfo[ jinfo$ISO_ABBREV == "Urol.Int.", "ISO_ABBREV" ] <- "urol int"
jinfo[ jinfo$ISO_ABBREV == "Can. Med. Assoc. J.", "ISO_ABBREV" ] <- "cmaj"
jinfo[ jinfo$ISO_ABBREV == "Blood Pressure", "ISO_ABBREV" ] <- "blood press"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Roentgenol.", "ISO_ABBREV" ] <- "ajr am j roentgenol"
jinfo[ jinfo$ISO_ABBREV == "Human Vaccines Immunother.", "ISO_ABBREV" ] <- "hum vaccin immunother"
jinfo[ jinfo$ISO_ABBREV == "Oral Surg. Oral Med. Oral Pathol. Oral Radiol.", "ISO_ABBREV" ] <- "oral surg oral med oral pathol oral radiol endod"
jinfo[ jinfo$ISO_ABBREV == "Am. J. Physiol.-Endocrinol. Metab.", "ISO_ABBREV" ] <- "am j physiol endocrinol metab"
jinfo[ jinfo$ISO_ABBREV == "Injury-Int. J. Care Inj.", "ISO_ABBREV" ] <- "injury"
jinfo[ jinfo$ISO_ABBREV == "Fam. Pr.", "ISO_ABBREV" ] <- "fam pract"
jinfo[ jinfo$ISO_ABBREV == "Perfusion-UK", "ISO_ABBREV" ] <- "perfusion"
jinfo[ jinfo$ISO_ABBREV == "J. Ocular Pharmacol. Ther.", "ISO_ABBREV" ] <- "j ocul pharmacol ther"
jinfo[ jinfo$ISO_ABBREV == "Arthritis Care Res.", "ISO_ABBREV" ] <- "arthritis care res hoboken"
jinfo[ jinfo$ISO_ABBREV == "Indian Pediatrics", "ISO_ABBREV" ] <- "indian pediatr"
jinfo[ jinfo$ISO_ABBREV == "Gynecol.Obstet.Invest.", "ISO_ABBREV" ] <- "gynecol obstet invest"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Eating Disord.", "ISO_ABBREV" ] <- "int j eat disord"
jinfo[ jinfo$ISO_ABBREV == "J. Mimim. Invasive Gynecol.", "ISO_ABBREV" ] <- "j minim invasive gynecol"
jinfo[ jinfo$ISO_ABBREV == "Dermatology", "ISO_ABBREV" ] <- "dermatology basel"
jinfo[ jinfo$ISO_ABBREV == "Osteoarthritis Cartilage", "ISO_ABBREV" ] <- "osteoarthr cartil"
jinfo[ jinfo$ISO_ABBREV == "Clin. Pediatr.", "ISO_ABBREV" ] <- "clin pediatr phila"
jinfo[ jinfo$ISO_ABBREV == "J. Refractive Surg.", "ISO_ABBREV" ] <- "j refract surg"
jinfo[ jinfo$ISO_ABBREV == "Cancer Prev. Res.", "ISO_ABBREV" ] <- "cancer prev res phila"
jinfo[ jinfo$ISO_ABBREV == "J. Viral Hepatitis", "ISO_ABBREV" ] <- "j viral hepat"
jinfo[ jinfo$ISO_ABBREV == "J. Cardiovasc. Surg.", "ISO_ABBREV" ] <- "j cardiovasc surg torino"
jinfo[ jinfo$ISO_ABBREV == "South.Med.J.", "ISO_ABBREV" ] <- "south med j"
jinfo[ jinfo$ISO_ABBREV == "Aids Res. Hum. Retrovir.", "ISO_ABBREV" ] <- "aids res hum retroviruses"
jinfo[ jinfo$ISO_ABBREV == "J. Womens Health", "ISO_ABBREV" ] <- "j womens health larchmt"
jinfo[ jinfo$ISO_ABBREV == "J. Diabetes Complications", "ISO_ABBREV" ] <- "j diabetes complicat"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Chronic Obstr. Pulm. Dis.", "ISO_ABBREV" ] <- "int j chron obstruct pulmon dis"
jinfo[ jinfo$ISO_ABBREV == "JCPSP-J. Coll. Physicians Surg.", "ISO_ABBREV" ] <- "j coll physicians surg pak"
jinfo[ jinfo$ISO_ABBREV == "J. Hand Surg.-Am. Vol.", "ISO_ABBREV" ] <- "j hand surg am"
jinfo[ jinfo$ISO_ABBREV == "Circ.-Cardiovasc. Interv.", "ISO_ABBREV" ] <- "circ cardiovasc interv"
jinfo[ jinfo$ISO_ABBREV == "Plos Neglect. Trop. Dis.", "ISO_ABBREV" ] <- "plos negl trop dis"
jinfo[ jinfo$ISO_ABBREV == "Acta Ophthalmol.", "ISO_ABBREV" ] <- "acta ophthalmol scand"
jinfo[ jinfo$ISO_ABBREV == "Circ.-Heart Fail.", "ISO_ABBREV" ] <- "circ heart fail"
jinfo[ jinfo$ISO_ABBREV == "J. Laparoendosc. Adv. Surg. Tech.", "ISO_ABBREV" ] <- "j laparoendosc adv surg tech a"
jinfo[ jinfo$ISO_ABBREV == "Clinics", "ISO_ABBREV" ] <- "clinics sao paulo"
jinfo[ jinfo$ISO_ABBREV == "Int. J. Sport Physiol. Perform.", "ISO_ABBREV" ] <- "int j sports physiol perform"
jinfo[ jinfo$ISO_ABBREV == "Eur. J. Orthodont.", "ISO_ABBREV" ] <- "eur j orthod"
jinfo[ jinfo$ISO_ABBREV == "Milit. Med.", "ISO_ABBREV" ] <- "mil med"
jinfo[ jinfo$ISO_ABBREV == "Surg. Laparosc. Endosc. Pct. Tech.", "ISO_ABBREV" ] <- "surg laparosc endosc percutan tech"
jinfo[ jinfo$ISO_ABBREV == "Eur. Resp. J.", "ISO_ABBREV" ] <- "eur respir j"
jinfo[ jinfo$ISO_ABBREV == "Psychopharmacology", "ISO_ABBREV" ] <- "psychopharmacology berl"
jinfo[ jinfo$ISO_ABBREV == "Diabetic Med.", "ISO_ABBREV" ] <- "diabet med"
jinfo[ jinfo$ISO_ABBREV == "Menopause-J. N. Am. Menopause Soc.", "ISO_ABBREV" ] <- "menopause"
jinfo[ jinfo$ISO_ABBREV == "J. Subst. Abus. Treat.", "ISO_ABBREV" ] <- "j subst abuse treat"


# other abberations in jcat file
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Intern. Med.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch intern med"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Psychiatry", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch gen psychiat"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Psychiatry", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch gen psychiatry"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Ophthalmol.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch ophthalmol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Pediatr.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch pediatr adolesc med"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Surg.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch surg"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Dermatol.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch dermatol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Neurol.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch neurol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "JAMA Otolaryngol-Head Neck Surg.", ] # changed name in 2005
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "arch otolaryngol head neck surg"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Bone Joint J.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "j bone joint surg br"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "J. Acad. Nutr. Diet.", ] # changed name in 2012
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "j am diet assoc"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "J. Trauma Acute Care Surg.", ] # changed name in 2012
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "j trauma"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Contemp. Clin. Trials", ] # changed name in 2005
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "control clin trials"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "BJU Int.", ] # changed name in 2005
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "br j urol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Region. Anesth. Pain Med.", ] # changed name in 1998
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "reg anesth"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Heart", ] # changed name in 1996
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "br heart j"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Scand. J. Urol.", ] # changed name in 2013
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "scand j urol nephrol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "J. Hypertens.", ]
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "j hypertens suppl"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Scand. J. Gastroenterol.", ]
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "scand j gastroenterol suppl"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "RHEUMATOLOGY", ] # changed name in 1999
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "br j rheumatol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Obesity", ] # changed name in 2006
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "obes res"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Eur. J. Prev. Cardiol.", ] # changed name in 2006
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "eur j cardiovasc prev rehabil"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Eur. J. Anaesth.", ] # two notations
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "eur j anaesthesiol"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "br med j"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "bmj"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "BMJ-British Medical Journal", ] # three notations
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "br med j (clin res ed)"
jcat[ length(jcat[,1]) + 1,  ] <- jcat[ jcat$ISO_ABBREV == "Obesity", ] # two notations
jcat[ length(jcat[,1]), "ISO_ABBREV" ] <- "obesity silver spring"


jcat[ jcat$ISO_ABBREV == "BMJ-British Medical Journal", "ISO_ABBREV" ] <- "BMJ"
jcat[ jcat$ISO_ABBREV == "JAMA-J. Am. Med. Assoc.", "ISO_ABBREV" ] <- "jama"
jcat[ jcat$ISO_ABBREV == "JAIDS-JOURNAL OF ACQUIRED IMMUNE DEFICIENCY SYNDROMES", "ISO_ABBREV" ] <- "jaids"
jcat[ jcat$ISO_ABBREV == "Am. J. Psychiat.", "ISO_ABBREV" ] <- "am j psychiatry"
jcat[ jcat$ISO_ABBREV == "Can. J. Anesth.", "ISO_ABBREV" ] <- "can j anaesth" # wrongly indicated by PUBMED
jcat[ jcat$ISO_ABBREV == "Eur. Resp. J.", "ISO_ABBREV" ] <- "eur respir j" # wrongly indicated by PUBMED
jcat[ jcat$ISO_ABBREV == "J. Periodont.", "ISO_ABBREV" ] <- "j periodontol" # wrongly indicated by PUBMED
jcat[ jcat$ISO_ABBREV == "J. Bone Joint Surg.-Am. Vol.", "ISO_ABBREV" ] <- "j bone joint surg am"
jcat[ jcat$ISO_ABBREV == "Metab.-Clin. Exp.", "ISO_ABBREV" ] <- "metab clin exp"
jcat[ jcat$ISO_ABBREV == "JAIDS", "ISO_ABBREV" ] <- "j acquir immune defic syndr"
jcat[ jcat$ISO_ABBREV == "Pediatr.  Anesth.", "ISO_ABBREV" ] <- "paediatr anaesth"
jcat[ jcat$ISO_ABBREV == "Clin. Endocrinol.", "ISO_ABBREV" ] <- "clin endocrinol oxf"
jcat[ jcat$ISO_ABBREV == "Mennopause-J. N. Am. Menopause Soc.", "ISO_ABBREV" ] <- "menopause"
jcat[ jcat$ISO_ABBREV == "Int. J. Gynecol. Obstet.", "ISO_ABBREV" ] <- "int j gynaecol obstet"
jcat[ jcat$ISO_ABBREV == "Region. Anesth. Pain Med.", "ISO_ABBREV" ] <- "reg anesth pain med"
jcat[ jcat$ISO_ABBREV == "JNCI-J. Natl. Cancer Inst.", "ISO_ABBREV" ] <- "j natl cancer inst"
jcat[ jcat$ISO_ABBREV == "J. Obstet. Gynaecol.", "ISO_ABBREV" ] <- "j obstet gynaecol"
jcat[ jcat$ISO_ABBREV == "Osteoporosis Int.", "ISO_ABBREV" ] <- "osteoporos int"
jcat[ jcat$ISO_ABBREV == "J. Am. Acad. Child Adolesc. Psychiatr.", "ISO_ABBREV" ] <- "j am acad child adolesc psychiatry"
jcat[ jcat$ISO_ABBREV == "J. Arthroplast.", "ISO_ABBREV" ] <- "j arthroplasty"
jcat[ jcat$ISO_ABBREV == "J. Psychopharmacol.", "ISO_ABBREV" ] <- "j psychopharmacol oxford"
jcat[ jcat$ISO_ABBREV == "Clin. Orthop. Rel. Res.", "ISO_ABBREV" ] <- "clin orthop relat res"
jcat[ jcat$ISO_ABBREV == "PACE-Pacing Clin. Electrophysiol.", "ISO_ABBREV" ] <- "pacing clin electrophysiol"
jcat[ jcat$ISO_ABBREV == "Alcoholism (NY)", "ISO_ABBREV" ] <- "alcohol clin exp res"
jcat[ jcat$ISO_ABBREV == "World J.Surg.", "ISO_ABBREV" ] <- "world j surg"
jcat[ jcat$ISO_ABBREV == "J. Parenter. Enter. Nutr.", "ISO_ABBREV" ] <- "jpen j parenter enteral nutr"
jcat[ jcat$ISO_ABBREV == "Clin. Drug Invest.", "ISO_ABBREV" ] <- "clin drug investig"
jcat[ jcat$ISO_ABBREV == "RHEUMATOLOGY", "ISO_ABBREV" ] <- "rheumatology oxford"
jcat[ jcat$ISO_ABBREV == "Arch. Dis. Child.-Fetal Neonatal Ed.", "ISO_ABBREV" ] <- "arch dis child fetal neonatal ed"
jcat[ jcat$ISO_ABBREV == "Trans. Roy. Soc. Trop. Med. Hyg.", "ISO_ABBREV" ] <- "trans r soc trop med hyg"
jcat[ jcat$ISO_ABBREV == "Am. J. Orthod. Dentofac. Orthop.", "ISO_ABBREV" ] <- "am j orthod dentofacial orthop"
jcat[ jcat$ISO_ABBREV == "Eye", "ISO_ABBREV" ] <- "eye lond"
jcat[ jcat$ISO_ABBREV == "J. Sports Med. Phys. Fit.", "ISO_ABBREV" ] <- "j sports med phys fitness"
jcat[ jcat$ISO_ABBREV == "Hum. Psychopharmacol.-Clin. Exp.", "ISO_ABBREV" ] <- "hum psychopharmacol"
jcat[ jcat$ISO_ABBREV == "Int. J. Obes.", "ISO_ABBREV" ] <- "int j obes lond"
jcat[ jcat$ISO_ABBREV == "SAMJ S. Afr. Med. J.", "ISO_ABBREV" ] <- "s afr med j"
jcat[ jcat$ISO_ABBREV == "Am. J. Clin. Oncol.-Cancer Clin. Trials", "ISO_ABBREV" ] <- "am j clin oncol"
jcat[ jcat$ISO_ABBREV == "Antivir. Ther.", "ISO_ABBREV" ] <- "antivir ther lond"
jcat[ jcat$ISO_ABBREV == "Clin. Oral Implant. Res.", "ISO_ABBREV" ] <- "clin oral implants res"
jcat[ jcat$ISO_ABBREV == "J. Matern.-Fetal Neonatal Med.", "ISO_ABBREV" ] <- "j matern fetal neonatal med"
jcat[ jcat$ISO_ABBREV == "JACC-Cardiovasc. Interv.", "ISO_ABBREV" ] <- "jacc cardiovasc interv"
jcat[ jcat$ISO_ABBREV == "J. Gerontol. Ser. A-Biol. Sci. Med. Sci.", "ISO_ABBREV" ] <- "j gerontol a biol sci med sci"
jcat[ jcat$ISO_ABBREV == "Am. J. Geriatr. Psychiatr.", "ISO_ABBREV" ] <- "am j geriatr psychiatry"
jcat[ jcat$ISO_ABBREV == "J. Psychosomat. Res.", "ISO_ABBREV" ] <- "j psychosom res"
jcat[ jcat$ISO_ABBREV == "J. Manip. Physiol. Ther.", "ISO_ABBREV" ] <- "j manipulative physiol ther"
jcat[ jcat$ISO_ABBREV == "Int. J. Geriatr. Psychiatr.", "ISO_ABBREV" ] <- "int j geriatr psychiatry"
jcat[ jcat$ISO_ABBREV == "J. Dermatol. Treat.", "ISO_ABBREV" ] <- "j dermatolog treat"
jcat[ jcat$ISO_ABBREV == "Nutr. Metab. Carbiovasc. Dis.", "ISO_ABBREV" ] <- "nutr metab cardiovasc dis"
jcat[ jcat$ISO_ABBREV == "Eur. Arch. Oto-Rhino-Laryn.", "ISO_ABBREV" ] <- "eur arch otorhinolaryngol"
jcat[ jcat$ISO_ABBREV == "Int. J. Clin. Pharmacol. Ther. Toxicol.", "ISO_ABBREV" ] <- "int j clin pharmacol ther"
jcat[ jcat$ISO_ABBREV == "Acta Derm.-Venereol.", "ISO_ABBREV" ] <- "acta derm venereol"
jcat[ jcat$ISO_ABBREV == "Psycho-Oncol.", "ISO_ABBREV" ] <- "psychooncology"
jcat[ jcat$ISO_ABBREV == "Urol.Int.", "ISO_ABBREV" ] <- "urol int"
jcat[ jcat$ISO_ABBREV == "Can. Med. Assoc. J.", "ISO_ABBREV" ] <- "cmaj"
jcat[ jcat$ISO_ABBREV == "Blood Pressure", "ISO_ABBREV" ] <- "blood press"
jcat[ jcat$ISO_ABBREV == "Am. J. Roentgenol.", "ISO_ABBREV" ] <- "ajr am j roentgenol"
jcat[ jcat$ISO_ABBREV == "Human Vaccines Immunother.", "ISO_ABBREV" ] <- "hum vaccin immunother"
jcat[ jcat$ISO_ABBREV == "Oral Surg. Oral Med. Oral Pathol. Oral Radiol.", "ISO_ABBREV" ] <- "oral surg oral med oral pathol oral radiol endod"
jcat[ jcat$ISO_ABBREV == "Am. J. Physiol.-Endocrinol. Metab.", "ISO_ABBREV" ] <- "am j physiol endocrinol metab"
jcat[ jcat$ISO_ABBREV == "Injury-Int. J. Care Inj.", "ISO_ABBREV" ] <- "injury"
jcat[ jcat$ISO_ABBREV == "Fam. Pr.", "ISO_ABBREV" ] <- "fam pract"
jcat[ jcat$ISO_ABBREV == "Perfusion-UK", "ISO_ABBREV" ] <- "perfusion"
jcat[ jcat$ISO_ABBREV == "J. Ocular Pharmacol. Ther.", "ISO_ABBREV" ] <- "j ocul pharmacol ther"
jcat[ jcat$ISO_ABBREV == "Arthritis Care Res.", "ISO_ABBREV" ] <- "arthritis care res hoboken"
jcat[ jcat$ISO_ABBREV == "Indian Pediatrics", "ISO_ABBREV" ] <- "indian pediatr"
jcat[ jcat$ISO_ABBREV == "Gynecol.Obstet.Invest.", "ISO_ABBREV" ] <- "gynecol obstet invest"
jcat[ jcat$ISO_ABBREV == "Int. J. Eating Disord.", "ISO_ABBREV" ] <- "int j eat disord"
jcat[ jcat$ISO_ABBREV == "J. Mimim. Invasive Gynecol.", "ISO_ABBREV" ] <- "j minim invasive gynecol"
jcat[ jcat$ISO_ABBREV == "Dermatology", "ISO_ABBREV" ] <- "dermatology basel"
jcat[ jcat$ISO_ABBREV == "Osteoarthritis Cartilage", "ISO_ABBREV" ] <- "osteoarthr cartil"
jcat[ jcat$ISO_ABBREV == "Clin. Pediatr.", "ISO_ABBREV" ] <- "clin pediatr phila"
jcat[ jcat$ISO_ABBREV == "J. Refractive Surg.", "ISO_ABBREV" ] <- "j refract surg"
jcat[ jcat$ISO_ABBREV == "Cancer Prev. Res.", "ISO_ABBREV" ] <- "cancer prev res phila"
jcat[ jcat$ISO_ABBREV == "J. Viral Hepatitis", "ISO_ABBREV" ] <- "j viral hepat"
jcat[ jcat$ISO_ABBREV == "J. Cardiovasc. Surg.", "ISO_ABBREV" ] <- "j cardiovasc surg torino"
jcat[ jcat$ISO_ABBREV == "South.Med.J.", "ISO_ABBREV" ] <- "south med j"
jcat[ jcat$ISO_ABBREV == "Aids Res. Hum. Retrovir.", "ISO_ABBREV" ] <- "aids res hum retroviruses"
jcat[ jcat$ISO_ABBREV == "J. Womens Health", "ISO_ABBREV" ] <- "j womens health larchmt"
jcat[ jcat$ISO_ABBREV == "J. Diabetes Complications", "ISO_ABBREV" ] <- "j diabetes complicat"
jcat[ jcat$ISO_ABBREV == "Int. J. Chronic Obstr. Pulm. Dis.", "ISO_ABBREV" ] <- "int j chron obstruct pulmon dis"
jcat[ jcat$ISO_ABBREV == "JCPSP-J. Coll. Physicians Surg.", "ISO_ABBREV" ] <- "j coll physicians surg pak"
jcat[ jcat$ISO_ABBREV == "J. Hand Surg.-Am. Vol.", "ISO_ABBREV" ] <- "j hand surg am"
jcat[ jcat$ISO_ABBREV == "Circ.-Cardiovasc. Interv.", "ISO_ABBREV" ] <- "circ cardiovasc interv"
jcat[ jcat$ISO_ABBREV == "Plos Neglect. Trop. Dis.", "ISO_ABBREV" ] <- "plos negl trop dis"
jcat[ jcat$ISO_ABBREV == "Acta Ophthalmol.", "ISO_ABBREV" ] <- "acta ophthalmol scand"
jcat[ jcat$ISO_ABBREV == "Circ.-Heart Fail.", "ISO_ABBREV" ] <- "circ heart fail"
jcat[ jcat$ISO_ABBREV == "J. Laparoendosc. Adv. Surg. Tech.", "ISO_ABBREV" ] <- "j laparoendosc adv surg tech a"
jcat[ jcat$ISO_ABBREV == "Clinics", "ISO_ABBREV" ] <- "clinics sao paulo"
jcat[ jcat$ISO_ABBREV == "Int. J. Sport Physiol. Perform.", "ISO_ABBREV" ] <- "int j sports physiol perform"
jcat[ jcat$ISO_ABBREV == "Eur. J. Orthodont.", "ISO_ABBREV" ] <- "eur j orthod"
jcat[ jcat$ISO_ABBREV == "Milit. Med.", "ISO_ABBREV" ] <- "mil med"
jcat[ jcat$ISO_ABBREV == "Surg. Laparosc. Endosc. Pct. Tech.", "ISO_ABBREV" ] <- "surg laparosc endosc percutan tech"
jcat[ jcat$ISO_ABBREV == "Psychopharmacology", "ISO_ABBREV" ] <- "psychopharmacology berl"
jcat[ jcat$ISO_ABBREV == "Diabetic Med.", "ISO_ABBREV" ] <- "diabet med"
jcat[ jcat$ISO_ABBREV == "Menopause-J. N. Am. Menopause Soc.", "ISO_ABBREV" ] <- "menopause"
jcat[ jcat$ISO_ABBREV == "J. Subst. Abus. Treat.", "ISO_ABBREV" ] <- "j subst abuse treat"


# all journal names to lowercase
journals$ISO_ABBREV <- tolower(journals$ISO_ABBREV)
jinfo$ISO_ABBREV <- tolower(jinfo$ISO_ABBREV)
jinfo$TITLE <- tolower(jinfo$TITLE)
jinfo$Title20 <- tolower(jinfo$Title20)
jcat$ISO_ABBREV <- tolower(jcat$ISO_ABBREV)

# change & into AND
jinfo$TITLE <- gsub( "&", "and", jinfo$TITLE )
jcat$TITLE <- gsub( "&", "and", jcat$TITLE )

# remove punctuation from journal names
journals$ISO_ABBREV <- gsub( "[[:punct:]]", "", journals$ISO_ABBREV )
jinfo$ISO_ABBREV <- gsub( "[[:punct:]]", "", jinfo$ISO_ABBREV )
jinfo$TITLE <- gsub( "[[:punct:]]", "", jinfo$TITLE )
jinfo$Title20 <- gsub( "[[:punct:]]", "", jinfo$Title20 )
jcat$ISO_ABBREV <- gsub( "[[:punct:]]", "", jcat$ISO_ABBREV )

# nieuwe variabelen bij 'journals' van jinfo: ISO_ABBREV, TITLE, ISSUES.YEAR, 
#   COUNTRY.REGION, PUBLISHER_NAME
jnew <- plyr::join( journals,
                    jinfo,
                    by = "ISO_ABBREV", 
                    match = "first" )

# add 'catNew' to jnew
jnew <- plyr::join( jnew,
                    jcat[,c("ISO_ABBREV", "catNew")],
                    by = "ISO_ABBREV",
                    match = "first" )




# read files with journal impact factors
IFfilez <- list.files("O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/IF" )
for( i in 1:length(IFfilez) ) {
  # filename
  IF <- IFfilez[ i ]
  
  # extract year from filename
  year <- as.numeric( str_extract_all( IF, "[0-9][0-9][0-9][0-9]") )
  
  # read file
  fileyear <- read.csv( 
    #paste0( "O:/psychiatrie/predictie verantwoordonderzoek/10.predictors/journalIF/IF/", IF ),
      paste0( "IF/", IF ),
      stringsAsFactors = F,
    skip = 1 )  
  
  # remove last 2 lines "Copyright ..., By exporting data ..."
  fileyear <- fileyear[ c( 1:( length( fileyear[,1] ) - 2 ) ), ]
  

  # all journal names to lowercase & remove punctuation from journal names
  fileyear$Full.Journal.Title <- tolower(fileyear$Full.Journal.Title)
  fileyear$Full.Journal.Title <- gsub( "&", "and", fileyear$Full.Journal.Title )
  fileyear$Full.Journal.Title <- gsub( "[[:punct:]]", "", fileyear$Full.Journal.Title )
  
  # change necessary journal names
  fileyear[ fileyear$Full.Journal.Title == "british medical journal", "Full.Journal.Title" ] <- "bmjbritish medical journal"
  fileyear[ fileyear$Full.Journal.Title == "archives of internal medicine", "Full.Journal.Title" ] <- "jama internal medicine"
  fileyear[ fileyear$Full.Journal.Title == "archives of general psychiatry", "Full.Journal.Title" ] <- "jama psychiatry"
  fileyear[ fileyear$Full.Journal.Title == "archives of ophtalmology", "Full.Journal.Title" ] <- "jama ophtalmology"
  fileyear[ fileyear$Full.Journal.Title == "archives of pediatric and adolescent medicine", "Full.Journal.Title" ] <- "jama pediatrics"
  fileyear[ fileyear$Full.Journal.Title == "archives of surgery", "Full.Journal.Title" ] <- "jama surgery"
  fileyear[ fileyear$Full.Journal.Title == "archives of dermatology", "Full.Journal.Title" ] <- "jama dermatology"
  fileyear[ fileyear$Full.Journal.Title == "archives of neurology", "Full.Journal.Title" ] <- "jama neurology"
  fileyear[ fileyear$Full.Journal.Title == "archives of otolaryngologyhead and neck surgery", "Full.Journal.Title" ] <- "jama otolaryngologyhead  neck surgery"
  fileyear[ fileyear$Full.Journal.Title == "journal of bone and joint surgerybritish volume", "Full.Journal.Title" ] <- "bone and joint journal"
  fileyear[ fileyear$Full.Journal.Title == "journal of the american dietetic association", "Full.Journal.Title" ] <- "journal of the academy of nutrition and dietetics"
  fileyear[ fileyear$Full.Journal.Title == "controlled clinical trials", "Full.Journal.Title" ] <- "contemporary clinical trials"
  fileyear[ fileyear$Full.Journal.Title == "journal of traumainjury infection and critical care", "Full.Journal.Title" ] <- "journal of trauma and acute care surgery"
  fileyear[ fileyear$Full.Journal.Title == "regional anesthesia", "Full.Journal.Title" ] <- "regional anesthesia and pain medicine"
  fileyear[ fileyear$Full.Journal.Title == "scandinavian journal of urology and nephrology", "Full.Journal.Title" ] <- "scandinavian journal of urology"
  fileyear[ fileyear$Full.Journal.Title == "british journal of rheumatology", "Full.Journal.Title" ] <- "rheumatology"
  fileyear[ fileyear$Full.Journal.Title == "canadian journal of anaesthesiajournal canadien d anesthesie", "Full.Journal.Title" ] <- "canadian journal of anesthesiajournal canadien d anesthesie"
  fileyear[ fileyear$Full.Journal.Title == "journal of the national cancer institute", "Full.Journal.Title" ] <- "jncijournal of the national cancer institute"
  fileyear[ fileyear$Full.Journal.Title == "european journal of obstetrics gynecology and reproductive biology", "Full.Journal.Title" ] <- "european journal of obstetrics and gynecology and reproductive biology"
  fileyear[ fileyear$Full.Journal.Title == "archives of pediatrics and adolescent medicine", "Full.Journal.Title" ] <- "jama pediatrics"
  fileyear[ fileyear$Full.Journal.Title == "paediatric anaesthesia", "Full.Journal.Title" ] <- "pediatric anesthesia"
  fileyear[ fileyear$Full.Journal.Title == "south african medical journal", "Full.Journal.Title" ] <- "samj south african medical journal"
  fileyear[ fileyear$Full.Journal.Title == "jama otolaryngologyhead  neck surgery", "Full.Journal.Title" ] <- "jama otolaryngologyhead and neck surgery"
  fileyear[ fileyear$Full.Journal.Title == "oral surgery oral medicine oral pathology oral radiology and endodontology", "Full.Journal.Title" ] <- "oral surgery oral medicine oral pathology oral radiology"
  fileyear[ fileyear$Full.Journal.Title == "oral surgery oral medicine oral pathology oral radiology and endodontics", "Full.Journal.Title" ] <- "oral surgery oral medicine oral pathology oral radiology"
  fileyear[ fileyear$Full.Journal.Title == "applied physiology nutrition and metabolismphysiologie appliquee nutrition et metabolisme", "Full.Journal.Title" ] <- "applied physiology nutrition and metabolism"
  fileyear[ fileyear$Full.Journal.Title == "british journal of obstetrics and gynaecology", "Full.Journal.Title" ] <- "bjogan international journal of obstetrics and gynaecology"
  fileyear[ fileyear$Full.Journal.Title == "acta ophthalmologica scandinavica", "Full.Journal.Title" ] <- "acta ophthalmologica"
  fileyear[ fileyear$Full.Journal.Title == "obesity research", "Full.Journal.Title" ] <- "obesity"
  fileyear[ fileyear$Full.Journal.Title == "european journal of cardiovascular prevention and rehabilitation", "Full.Journal.Title" ] <- "european journal of preventive cardiology"
  fileyear[ fileyear$Full.Journal.Title == "journal of cardiovascular risk", "Full.Journal.Title" ] <- "european journal of preventive cardiology"

  
  # remove duplicate rows
  fileyear <- fileyear[ !duplicated(fileyear$Full.Journal.Title),]
  
  # name of dataframe and variable
  n <- paste0( "IF", year )
  names(fileyear)[4] <- n
  fileyear[,4] <- as.numeric( fileyear[,4])
  
  # outframe
  outframe <- paste0( n, " <- fileyear")
  
  # create dataframe IF<year>
  eval( parse( text = outframe ) )
  
  rm(fileyear)
}



# combine IFs
IFtotal <- data.frame( Full.Journal.Title = IF2016$Full.Journal.Title,
                           IF2016 = IF2016$IF2016,
                           stringsAsFactors = F ) 
IFtotal <- plyr::join_all( dfs = list(IFtotal, 
                  IF2015[,c("Full.Journal.Title", "IF2015")]),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list( IFtotal, 
                  IF2014[,c("Full.Journal.Title", "IF2014")]),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list( IFtotal, 
                  IF2013[,c("Full.Journal.Title", "IF2013")]),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal, 
                  IF2012[,c("Full.Journal.Title", "IF2012")]),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2011[,c("Full.Journal.Title", "IF2011")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" ) 
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2010[,c("Full.Journal.Title", "IF2010")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2009[,c("Full.Journal.Title", "IF2009")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2008[,c("Full.Journal.Title", "IF2008")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2007[,c("Full.Journal.Title", "IF2007")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2006[,c("Full.Journal.Title", "IF2006")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2005[,c("Full.Journal.Title", "IF2005")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2004[,c("Full.Journal.Title", "IF2004")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2003[,c("Full.Journal.Title", "IF2003")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2002[,c("Full.Journal.Title", "IF2002")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2001[,c("Full.Journal.Title", "IF2001")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF2000[,c("Full.Journal.Title", "IF2000")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF1999[,c("Full.Journal.Title", "IF1999")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF1998[,c("Full.Journal.Title", "IF1998")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )
IFtotal <- plyr::join_all( dfs = list(IFtotal,  
                  IF1997[,c("Full.Journal.Title", "IF1997")] ),
                  by = "Full.Journal.Title", match = "all", type = "full" )





# change catNew for those where it is obvious: journal title contains 'psychiatry'? <- psychiatrie; etc.
jnew[ jnew$catNew == "other" & 
           grepl("immunology", jnew$TITLE), "catNew" ] <- "immunology"
jnew[ jnew$catNew =="other" & 
           grepl("anesthesiology|anaesthesiology", jnew$TITLE), "catNew" ] <- "anesthesiology"
jnew[ jnew$catNew =="other" & 
           grepl("cardiovascular|heart", jnew$TITLE), "catNew" ] <- "heart_vascular"
jnew[ jnew$catNew =="other" & 
           grepl("neurology", jnew$TITLE), "catNew" ] <- "neurology"
jnew[ jnew$catNew =="other" & 
           grepl("dermatology|skin", jnew$TITLE), "catNew" ] <- "dermatology"
jnew[ jnew$catNew =="other" & 
           grepl("endocrin|metaboli", jnew$TITLE), "catNew" ] <- "endocrinology_metabolism"
jnew[ jnew$catNew =="other" & 
           grepl("entero|hepato", jnew$TITLE), "catNew" ] <- "gastroenterology_hepatology"
jnew[ jnew$catNew =="other" & 
           grepl("genetc|heredity", jnew$TITLE), "catNew" ] <- "genetics_heredity"
jnew[ jnew$catNew =="other" & 
           grepl("geriatr|gerontol", jnew$TITLE), "catNew" ] <- "geriatrics_gerontology"
jnew[ jnew$catNew =="other" & 
           grepl("internal", jnew$TITLE), "catNew" ] <- "general"
jnew[ jnew$catNew =="other" & 
           grepl("hematology", jnew$TITLE), "catNew" ] <- "hematology"
jnew[ jnew$catNew =="other" & 
           grepl("infectious", jnew$TITLE), "catNew" ] <- "infectious"
jnew[ jnew$catNew =="other" & 
           grepl("integrative|holistic|complementary", jnew$TITLE), "catNew" ] <- "integrative_complementary"
jnew[ jnew$catNew =="other" & 
           grepl("gynacology|gynaecol|obstetr", jnew$TITLE), "catNew" ] <- "gynacology_reproduction"
jnew[ jnew$catNew =="other" & 
           grepl("oncology|cancer|neoplasm", jnew$TITLE), "catNew" ] <- "oncology"
jnew[ jnew$catNew =="other" & 
           grepl("ophthalmolo", jnew$TITLE), "catNew" ] <- "ophthalmology"
jnew[ jnew$catNew =="other" & 
           grepl("orthopedic", jnew$TITLE), "catNew" ] <- "orthopedics"
jnew[ jnew$catNew =="other" & 
           grepl("pediatric|paediatric", jnew$TITLE), "catNew" ] <- "pediatrics"
jnew[ jnew$catNew =="other" & 
           grepl("psychiatry|psychopharmacol", jnew$TITLE), "catNew" ] <- "psychiatry"
jnew[ jnew$catNew =="other" & 
           grepl("rehabilitation", jnew$TITLE), "catNew" ] <- "rehabilitation"
jnew[ jnew$catNew =="other" & 
           grepl("lung|respirato", jnew$TITLE), "catNew" ] <- "respiratory_system"
jnew[ jnew$catNew =="other" & 
           grepl("surgery", jnew$TITLE), "catNew" ] <- "surgery"
jnew[ jnew$catNew =="other" & 
           grepl("urol|nephrol", jnew$TITLE), "catNew" ] <- "urology_nephrology"



jnew[ is.na(jnew$catNew) & grepl("immunol", jnew$ISO_ABBREV), "catNew" ] <- "immunology"
jnew[ is.na(jnew$catNew) & grepl("anesth|anaesth", jnew$ISO_ABBREV), "catNew" ] <- "anesthesiology"
jnew[ is.na(jnew$catNew) & grepl("cardiovasc|heart", jnew$ISO_ABBREV), "catNew" ] <- "heart_vascular"
jnew[ is.na(jnew$catNew) & grepl("neurol", jnew$ISO_ABBREV), "catNew" ] <- "neurology"
jnew[ is.na(jnew$catNew) & grepl("dermatol|skin", jnew$ISO_ABBREV), "catNew" ] <- "dermatology"
jnew[ is.na(jnew$catNew) & grepl("endocrin|metab", jnew$ISO_ABBREV), "catNew" ] <- "endocrinology_metabolism"
jnew[ is.na(jnew$catNew) & grepl("entero|hepato", jnew$ISO_ABBREV), "catNew" ] <- "gastroenterology_hepatology"
jnew[ is.na(jnew$catNew) & grepl("genetc|heredity", jnew$ISO_ABBREV), "catNew" ] <- "genetics_heredity"
jnew[ is.na(jnew$catNew) & grepl("geriatr|gerontol", jnew$ISO_ABBREV), "catNew" ] <- "geriatrics_gerontology"
jnew[ is.na(jnew$catNew) & grepl("internal", jnew$ISO_ABBREV), "catNew" ] <- "general"
jnew[ is.na(jnew$catNew) & grepl("hematol", jnew$ISO_ABBREV), "catNew" ] <- "hematology"
jnew[ is.na(jnew$catNew) & grepl("infect", jnew$ISO_ABBREV), "catNew" ] <- "infectious"
jnew[ is.na(jnew$catNew) & grepl("integr|holistic|complementary", jnew$ISO_ABBREV), "catNew" ] <- "integrative_complementary"
jnew[ is.na(jnew$catNew) & grepl("gynacol|gynaecol|obstetr", jnew$ISO_ABBREV), "catNew" ] <- "gynacology_reproduction"
jnew[ is.na(jnew$catNew) & grepl("oncol|cancer|neoplasm", jnew$ISO_ABBREV), "catNew" ] <- "oncology"
jnew[ is.na(jnew$catNew) & grepl("ophthalmo", jnew$ISO_ABBREV), "catNew" ] <- "ophthalmology"
jnew[ is.na(jnew$catNew) & grepl("orthop", jnew$ISO_ABBREV), "catNew" ] <- "orthopedics"
jnew[ is.na(jnew$catNew) & grepl("pediat|paediat", jnew$ISO_ABBREV), "catNew" ] <- "pediatrics"
jnew[ is.na(jnew$catNew) & grepl("psychiat|psychopharmacol", jnew$ISO_ABBREV), "catNew" ] <- "psychiatry"
jnew[ is.na(jnew$catNew) & grepl("rehabil", jnew$ISO_ABBREV), "catNew" ] <- "rehabilitation"
jnew[ is.na(jnew$catNew) & grepl("lung|respirato", jnew$ISO_ABBREV), "catNew" ] <- "respiratory_system"
jnew[ is.na(jnew$catNew) & grepl("surg", jnew$ISO_ABBREV), "catNew" ] <- "surgery"
jnew[ is.na(jnew$catNew) & grepl("urol|nephrol", jnew$ISO_ABBREV), "catNew" ] <- "urology_nephrology"



# add variables to the 'jnew' dataframe: IF, deltaIF and category
jnew$deltaIF <- jnew$IF <- NA
print(Sys.time())
for( i in 1:length( jnew[ , 1 ] ) ) {
  if( i %in% seq( 1, 400000, 1000 ) ) { print( paste( "finished", i, "out of", length(jnew[,1]))) }
    
  # select journal of publication
  j2 <- jnew[ i, "TITLE" ]
  
  # add impact factor for specific year
  year <- jnew[ i, "yearpub" ] - 1
  if( !is.na( year ) )
  {
    if( year > 2016 ) year <- 2016
    if( year < 1997 ) year <- 1997
    colname <- paste0( "IF", year )
    if( !is.na( j2 ) ) {
      IF <- IFtotal[ IFtotal$Full.Journal.Title == j2, colname ]
      if( length(IF) > 1 ) IF <- IF[!is.na(IF)]
      if( length(IF) > 1 ) IF <- NA
      jnew[ i, ]$IF <- IF
    }
  
    # add deltaIF stability
    year2 <- year - 1
    if( year2 > 2016 ) year2 <- 2016
    if( year2 < 1997 ) year2 <- 1997
    colname2 <- paste0( "IF", year2 )
    if( !is.na( j2 ) ) {
      IF2 <- IFtotal[ IFtotal$Full.Journal.Title == j2, colname2 ]
      if( length(IF2) > 1 ) IF2 <- IF2[!is.na(IF2)]
      if( length(IF2) > 1 ) IF2 <- NA
      jnew[ i, ]$deltaIF <- IF - IF2
    }
  }
}


# summarize missing information
jNoCategory <- arrange(jnew[is.na(jnew$catNew),], ISO_ABBREV)
jNoIF <- arrange(jnew[is.na(jnew$IF),], ISO_ABBREV)
jNoDelta <- arrange(jnew[is.na(jnew$deltaIF),], ISO_ABBREV)
jNoDelta_title <- jNoDelta[ !is.na(jNoDelta$TITLE), ]
single_JNDT <- jNoDelta_title[ !duplicated(jNoDelta_title$ISO_ABBREV), ]

sum.noCat <- as.data.frame(table(jNoCategory$ISO_ABBREV))
sum.noIF <- as.data.frame(table(jNoIF$ISO_ABBREV))
sum.noDelta <- as.data.frame(table(jNoDelta$ISO_ABBREV))
sum.noDeltaTitle <- as.data.frame(table(jNoDelta_title$ISO_ABBREV))

sum.noCat <- arrange(sum.noCat, -Freq)
sum.noIF <- arrange(sum.noIF, -Freq)
sum.noDelta <- arrange(sum.noDelta, -Freq)
sum.noDeltaTitle <- arrange(sum.noDeltaTitle, -Freq)

test <- merge( sum.noDeltaTitle, single_JNDT, by.x = "Var1", by.y = "ISO_ABBREV" )
test <- arrange( test, -Freq )

# save to investigate
write.csv2( jnew, "journals_cat-IF-dIF.csv", row.names = F )
write.csv2( jNoCategory, "lookupNoCat.csv", row.names = F )
write.csv2( jNoDelta, "lookupNoDeltaIF.csv", row.names = F )
write.csv2( jNoIF, "lookupNoIF.csv", row.names = F )
write.csv2( sum.noCat, "lookupSummary.csv", row.names = F )
write.csv2( sum.noDelta, "lookupSummaryDelta.csv", row.names = F )
write.csv2( sum.noIF, "lookupSummaryIF.csv", row.names = F )

print(Sys.time())

# quit R session
#q( save = "no" )