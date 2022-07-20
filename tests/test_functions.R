source("0_functions.R")

# calculate_weights() ####
local({
  sour <- 1:10
  targ <- 1:5
  stopifnot(isTRUE(all.equal(
    round(mean(targ)),
    round(weighted.mean(sour, calculate_weights(sour, targ)))
  )))
})


# weighted.quantile() ####
local({
  x <- rnorm(100)
  stopifnot(isTRUE(all.equal(weighted.quantile(x, w = 1), quantile(x))))
})
local({
  # Example from man('weighted.mean')
  x <- c(3.7, 3.3, 3.5, 2.8)
  w <- c(5,   5,   4,   1)/15
  stopifnot(isTRUE(all.equal(
    weighted.quantile(x, w, 0:4/4, names = FALSE),
    c(2.8, 3.33611111111111, 3.46111111111111, 3.58157894736842,
      3.7)
  )))
})

# replace_ncr() ####
local({
  # We are not comparing decode_ncr(unname(with_ncr)) to
  # names(with_ncr) becase it doesn't work in Windows.
  with_ncr <- unname(c(
    # negative controls
    "&#" = "&#",
    "Fontenelle, Leonardo Ferreira" = "Fontenelle, Leonardo Ferreira",
    # one HTML numerical character reference (NCR)
    "\u0130zol, Bozan Serhat" = "&#304;zol, Bozan Serhat",
    # two NCR
    "Ke\u015Fkek, \u015Eakir \u00D6zg\u{00FC}r" = "Ke&#351;kek, &#350;akir Özgür",
    # One NCR at the end
    "Pluta, Micha\u0142" = "Pluta, Micha&#322;",
    # One NCR at the end, another in the middle
    "Tekiner, \u0130smail Hakk\u0131" = "Tekiner, &#304;smail Hakk&#305;",
    # Two NCR at the beginning
    "Buyuk, Suleyman Kutalm\u0131\u015F" = "Buyuk, Suleyman Kutalm&#305;&#351;",
    # Two NCR at the beginning
    "\u0130\u015Fcan, Yal\u0131n" = "&#304;&#351;can, Yal&#305;n",
    # Two NCR at the end, one at the beginning
    "\u015Eim\u015Fek, Bar\u0131\u015F" = "&#350;im&#351;ek, Bar&#305;&#351;"
  ))
  without_ncr <- c(
    # negative control
    "&#",
    "Fontenelle, Leonardo Ferreira",
    # one HTML numerical character reference (NCR)
    "\u0130zol, Bozan Serhat",
    # two NCR
    "Ke\u015Fkek, \u015Eakir \u00D6zg\u{00FC}r",
    # One NCR at the end
    "Pluta, Micha\u0142",
    # One NCR at the end, another in the middle
    "Tekiner, \u0130smail Hakk\u0131",
    # Two NCR at the beginning
    "Buyuk, Suleyman Kutalm\u0131\u015F",
    # Two NCR at the beginning
    "\u0130\u015Fcan, Yal\u0131n",
    # Two NCR at the end, one at the beginning
    "\u015Eim\u015Fek, Bar\u0131\u015F"
  )
  stopifnot(identical(decode_ncr(with_ncr), without_ncr))
})

# fix_encoding ####
local({
  from <- c(

    # Apostrophe
    "Sant\u00b4Anna",
    "D \u00cc\u0081Paula",
    "Cruzat\u05beMandich",

    # Hyphen
    "Fuica\u0096Rebolledo",

    # Vowel + acute
    "Azca\u00cc\u0081rate",
    "Jose\u00cc\u0081",
    "Mari\u00cc\u0081a",
    "Co\u00cc\u0081rdova",
    "Rau\u00cc\u0081l",
    "\u00cd\u0081ngel",
    "\u{009f}lvarez",

    # vowel + tilde
    "Gusma\u00cc\u0083o",
    "Simo\u00cc\u0083es",

    # vowel + circumflex
    "Vena\u00cc\u0082ncio",
    "Corre\u00cc\u0082a",
    "So\u00cc\u0082nia",

    # vowel + umlaut
    "Bu\u00cc\u0088ndchen",

    # c-cedilha
    "Gonc\u00cc\u00a7alves",

    # n-tilde
    "Meon\u00cc\u0083o",

    # consonant + comma
    "Arma\u00c8\u0099u",
    "Covan\u00c8\u009bev",

    # consonant + caron
    "Red\u{009e}epagi\u{0107}",

    # Miscellaneous
    "Jo\u00c3\u00a3o",
    "Ru\u00ef\u00ac\u0081no",
    "\u00c7eti\u00cc\u0087nkaya",
    "N\u02d9\u00d2ez",
    "Jad\u00b7n-Guerrero",
    "Lepe-Mart\u00ccnez",
    "G\u00dbmez-Garc\u00cca",
    "Rea\u00a4o",
    "O\u0094Relly",
    "\u00a8O\u00a8",
    "Miguel\u25cb",
    "O\u0092Farrill",
    "Pichardo-\u0412\u0430\u04bb\u0435na",

    # Control characters
    "\u0082",
    "\u0086"
  )
  to <- c(
    # Apostrophe
    "Sant'Anna",
    "D'Paula",
    "Cruzat-Mandich",

    # Hyphen
    "Fuica-Rebolledo",

    # Vowel + acute
    "Azc\u00e1rate",
    "Jos\u00e9",
    "Mar\u{00ed}a",
    "C\u{00f3}rdova",
    "Ra\u{00fa}l",
    "\u{00c1}ngel",
    "\u{00c1}lvarez",

    # vowel + tilde
    "Gusm\u{00e3}o",
    "Sim\u{00f5}es",

    # vowel + circumflex
    "Ven\u{00e2}ncio",
    "Corr\u{00ea}a",
    "S\u{00f4}nia",

    # vowel + umlaut
    "B\u{00fc}ndchen",

    # c-cedilha
    "Gon\u00e7alves",

    # n-tilda
    "Meo\u{00f1}o",

    # consonant + comma
    "Arma\u{0219}u",
    "Covan\u{021b}ev",

    # consonant + caron
    "Red\u{017e}epagi\u{0107}",

    # Miscellaneous
    "Jo\u{00e3}o",
    "Rufino",
    "\u{00c7}eti\u{0307}nkaya",
    "N\u00fa\u00f1ez",
    "Jad\u00e1n-Guerrero",
    "Lepe-Mart\u{00ed}nez",
    "G\u{00f3}mez-Garc\u{00ed}a",
    "Rea\u00f1o",
    "O'Relly",
    "O",
    "Miguelo",
    "O'Farrill",
    "Pichardo-Bahena",

    # Control characters
    "",
    ""
  )
  stopifnot(fix_encoding(from) == to)
  # cbind(fix_encoding(from), to)[fix_encoding(from) != to, ]
})


# fix_issn() ####
stopifnot(identical(
  fix_issn(c(
    "1809-5909", "2314-159X", "2314-159x", "XXXX-0161", "XXXX-0161",
    "15177130", "2317-269", "0104-40600102-4698", NA_character_)),
  c(
    "1809-5909", "2314-159X", "2314-159x", "XXXX-0161", "XXXX-0161",
    "1517-7130", "2317-269X", "0102-4698", NA_character_)
))


# fix_orcid() ####
local({
  orcid_sample <- c(
    # Two correct ones
    `0000-0001-7981-408X` = "0000-0001-7981-408X",
    `0000-0001-9859-4115` = "0000-0001-9859-4115",
    # Recurring errors
    `0000-0003-4657-2808` = "orcid.org/0000-0003-4657-2808",
    `0000-0002-8391-091X` = "orcid.org/0000-0002-8391-091X",
    `0000-0002-3764-9116` = "ORCID: 0000-0002-3764-9116",
    `0000-0002-5019-649X` = "ORCID: 0000-0002-5019-649X",
    `0000-0002-3787-919X` = "http://orcid.org/0000-0002-3787-919X",
    `0000-0002-2476-1731` = "http://orcid.org/0000-0002-2476-1731",
    `0000-0002-7297-1645` = ": 0000-0002-7297-1645",
    `0000-0002-2176-809X` = ": 0000-0002-2176-809X",
    `0000-0002-6639-8918` = "000-0002-6639-8918",
    `0000-0002-7869-540X` = "000-0002-7869-540X",
    `0000-0002-2021-903x` = "000.0002.2021.903x",
    `0000-0001-9857-0930` = "0000 0001 9857 0930",
    `0000-0002-5319-086X` = "0000 0002 5319 086X",
    `0000-0002-1751-2913` = "0000.0002.1751.291",
    `0000-0001-6782-693X` = "0000.0001.6782.693X",
    `0000-0001-5486-0194` = "00000-0001-5486-0194",
    `0000-0002-5333-013X` = "00000-0002-5333-013X",
    `0000-0001-5277-2860` = "0000­0001­5277­2860",
    `0000-0002-0580-234X` = "0000­0002­0580­234X",
    `0000-0001-9001-485X` = "0001 9001 485X",
    `0000-0002-2589-2813` = "0002 2589 2813",
    `0000-0003-0519-2868` = "0000- 0003-0519-2868",
    `0000-0002-6892-968X` = "0000-0002-6892- 968X",
    `0000-0002-4463-908X` = "0000-0002-4463908X",
    `0000-0003-4473-9678` = "0000-0003-44739678",
    # Verified typos
    `0000-0003-3484-9638` = "000.003.3484.9638",
    `0000-0003-2352-3915` = "0300-0003-2352-3915",
    `0000-0002-1751-2913` = "0000.0002.1751.291",
    `0000-0002-1273-2044` = "2044-0000-0002-1273-2044",
    `0000-0002-0277-4371` = "0000.0002.0277.437",
    "67A8-RN00-0000-0000",
    `0000-0003-4890-7122` = "0000-000-4890-7122",
    `0000-0001-6383-7981` = "0000-0001-6383-",
    `0000-0001-6400-4308` = "0000-0001-6400-43",
    `0000-0001-7457-7724` = "0000-0001-7457-772",
    `0000-0001-9402-561X` = "0000-0001-9402-561",
    `0000-0002-0151-3612` = "0000-0002-0151-361",
    `0000-0002-2112-530X` = "0000-0002-212-530X",
    `0000-0002-3373-8250` = "0000-0002-3373-825",
    `0000-0002-4259-3655` = "0000-0002-4259-3h655",
    "0000-0002-5470-055",
    `0000-0002-6215-0923` = "0000-0002-6215-09",
    `0000-0002-7745-0718` = "0000-0002-7745-071",
    `0000-0002-9631-1908` = "0000-0002-9631-190",
    `0000-0003-2088-4870` = "0000-0003-208+8-4870",
    `0000-0002-1387-1708` = "0000-0003-3286-870",
    `0000-0003-4467-1933` = "0000-0003-4467-193",
    `0000-0001-8309-4003` = "0000-000l-8309-4003",
    `0000-0002-0063-2011` = "0000-002-0063-2011",
    `0000-0003-0826-525x` = "0000-003-0826-525x",
    `0000-0003-2842-2267` = "0000-003-2842-2267"
  )
  stopifnot(identical(
    unname(fix_orcid(orcid_sample)),
    replace(names(orcid_sample), names(orcid_sample) %in% "", NA_character_)
  ))
})


# normalize_jdescr() ####

local({
  str <- rbind(
    c(test = "Sociologia", answer = "Sociologia"),
    # This is an unfortunate behavior of stringi::stri_trans_totitle()
    c("T\u00e9cnicas de Laborat\u00f3rio Cl\u00ednico",
      "T\u00e9cnicas De Laborat\u00f3rio Cl\u00ednico"),
    # this works with stringi::stri_trans_totitle(), not with tools::totitle()
    c("TECNOLOGIA, IND\u00daSTRIA, AGRICULTURA",
      "Tecnologia, Ind\u00fastria, Agricultura"),
    c("TERAPEUTICA", "Terap\u00eautica"),
    c("Terap\u00eautica", "Terap\u00eautica"),
    c("TRAUMATOLOGIA", "Traumatologia"),
    # When there's no version with diacritics
    c("VETERINARIA", "Veterinaria")
  )
  stopifnot(all.equal(normalize_jdescr(str[, "test"]), str[, "answer"]))
})

# split_subfields() ####

# Examples A through E are from
# http://metodologia.lilacs.bvs.br/download/I/LILACS-2-ManualDescricao-en.pdf
# The other ones are data obtained from LILACS
local({
  author_field_samples <- c(
    A = paste0("Silva, Regina^1Universidade Federal de São Paulo",
               "^2Escola Paulista de Medicina^3Departamento de Enfermagem. ",
               "Disciplina de Otorrinolaringologia. Sessão deFonética",
               "^pBrasil^cSão Paulo"),
    B = paste0("Greco, Luis Miguel^1Universidade Federal de São Paulo",
               "^pBrasil^cSão Paulo"),
    C = "Silva, Rodolfo^1s.af",
    D = "Gonçalves, Maria^1Hospital de los Niños^ps.p",
    E = "Catañedo, Juan^redt^1s.af",
    wrong_order = paste0("Soares Almeida, Anne Louise^3Department of Nursing",
                         "^2Health Science Center^1Federal University of Rio",
                         " Grande do Norte^pBR^cNatal"),
    with_orcid = paste0("Alcantara-Cortes, Johan Steven^k0000-0003-1176-2599",
                        "^1Universidad Colegio Mayor de Cundinamarca^pCO")
  )

  author_field_results <- list(
    A = c(name = "Silva, Regina",
          affiliation_1 = "Universidade Federal de São Paulo",
          affiliation_2 = "Escola Paulista de Medicina",
          affiliation_3 = paste0("Departamento de Enfermagem. Disciplina de",
                                 " Otorrinolaringologia. Sessão deFonética"),
          city = "São Paulo",
          state = NA_character_,
          country = "Brasil",
          orcid_id = NA_character_,
          responsibility = NA_character_),
    B = c(name = "Greco, Luis Miguel",
          affiliation_1 = "Universidade Federal de São Paulo",
          affiliation_2 = NA_character_,
          affiliation_3 = NA_character_,
          city =  "São Paulo",
          state = NA_character_,
          country = "Brasil",
          orcid_id = NA_character_,
          responsibility = NA_character_),
    C = c(name = "Silva, Rodolfo",
          affiliation_1 = "s.af",
          affiliation_2 = NA_character_,
          affiliation_3 = NA_character_,
          city = NA_character_,
          state = NA_character_,
          country = NA_character_,
          orcid_id = NA_character_,
          responsibility = NA_character_),
    D = c(name = "Gonçalves, Maria",
          affiliation_1 = "Hospital de los Niños",
          affiliation_2 = NA_character_,
          affiliation_3 = NA_character_,
          city = NA_character_,
          state = NA_character_,
          country = "s.p",
          orcid_id = NA_character_,
          responsibility = NA_character_),
    E = c(name = "Catañedo, Juan",
          affiliation_1 = "s.af",
          affiliation_2 = NA_character_,
          affiliation_3 = NA_character_,
          city = NA_character_,
          state = NA_character_,
          country = NA_character_,
          orcid_id = NA_character_,
          responsibility = "edt"),
    wrong_order = c(name = "Soares Almeida, Anne Louise",
                    affiliation_1 = "Federal University of Rio Grande do Norte",
                    affiliation_2 = "Health Science Center",
                    affiliation_3 = "Department of Nursing",
                    city = "Natal",
                    state = NA_character_,
                    country = "BR",
                    orcid_id = NA_character_,
                    responsibility = NA_character_),
    with_orcid = c(name = "Alcantara-Cortes, Johan Steven",
                   affiliation_1 = "Universidad Colegio Mayor de Cundinamarca",
                   affiliation_2 = NA_character_,
                   affiliation_3 = NA_character_,
                   city = NA_character_,
                   state = NA_character_,
                   country = "CO",
                   orcid_id = "0000-0003-1176-2599",
                   responsibility = NA_character_)
  )
  author_colnames <- c(
    "_" = "name",
    "1" = "affiliation_1",
    "2" = "affiliation_2",
    "3" = "affiliation_3",
    "c" = "city",
    "s" = "state",         # non-standard
    "p" = "country",
    "k" = "orcid_id",      # non-standard
    "r" = "responsibility"
  )
  for (name in names(author_field_samples)) {
    stopifnot(
      author_field_samples[[name]] |>
        split_subfields(tags = names(author_colnames)) |>
        setNames(nm = author_colnames) |>
        identical(author_field_results[[name]])
    )
  }
  # The TITLE database has
  stopifnot(split_subfields("SCOPUS", names(author_colnames))[["_"]] ==
             "SCOPUS")
})


# read_record() ####

tmp_filename <- tempfile(fileext = ".isis")

local({
  record_samples <- list(c(
    # Example in https://wiki.bireme.org/pt/img_auth.php/5/5f/2709BR.pdf
    "00433000000000121000450004400780000005000120007806900790009002400690016902600230",
    "0238030002100261070001600282070001300298#Methodology of plant eco-physiology: pr",
    "oceedings of the Montpellier Symposium#Incl. bibl.#Paper on: <plant physiology><",
    "plant transpiration><measurement and instruments>#Techniques for the measurement",
    " of transpiration of individual plants#^aParis^bUnesco^c-1965#^ap. 211-224^billu",
    "s.#Magalhaes, A.C.#Franco, C.M.##"
  ), c(
    # Real-world record
    "03174000000000697000450000100090000000200150000900500020002400600030002601001030",
    "00290100033001320120072001650130051002370140009002880300030002970310003003270320",
    "00200330035001000332040000300342064001000345065000900355067000300364071001700367",
    "07600070038408314900039108500400188108500220192108500190194308700360196208700270",
    "19980870016020250910020020410920012020610930020020731130002020934400033020957210",
    "00702128721003502135721000702170721001702177722000702194722002802201722000702229",
    "72200200223672300070225672300280226372300070229172300190229872500080231772500010",
    "23257250007023267250001023337760026023348540007023608550005023679040007023729040",
    "00702379904000402386098000602390778001502396008006502411#BR1764.1#biblio-1024035",
    "#S#as#Haby, Michelle M^2Department of Chemical and Biological Sciences^1Universi",
    "ty of Sonora^pMX^cHermosillo#Clark, Rachel^1s.a.f^pGB^cLondon#Respostas rápidas ",
    "para Políticas de Saúde Informadas por Evidências^ipt#Rapid response for evidenc",
    "e-informed health policy#^l42^f32#BIS, Bol. Inst. Saúde (Impr.)#17#1#1518-1812#p",
    "t#Jul. 2016#20160000#BR#Technical Report#Humans#É reconhecido amplamente que as ",
    "intervenções de saúde sustentadas pelas evidências de pesquisa serão mais efetiv",
    "as do que aquelas que não são. Entretanto, na formulação de políticas de saúde, ",
    "com seus processos decisórios não lineares e motivações políticas concorrentes, ",
    "o uso de evidências científicas para informar a tomada de decisão não é um proce",
    "sso fácil. As barreiras geralmente citadas incluem acesso limitado a pesquisas d",
    "e alta qualidade e baixa relevância e oportunidade de pesquisas, enquanto a maio",
    "r colaboração e relação entre pesquisadores e tomadores de decisão foram citadas",
    " como um importante facilitador para o uso das evidências científicas. Programas",
    " de respostas rápidas foram iniciados para superar essas barreiras e facilitar o",
    " uso da evidência científica, assegurando a provisão do acesso a resultados de p",
    "esquisas relevantes e oportunas e através do trabalho compartilhado entre pesqui",
    "sadores e gestores. Um programa normalmente funciona para prover sínteses de evi",
    "dências científicas em um curto período de tempo. Revisões rápidas da literatura",
    " são produtos-chave. Atualmente há uma grande variação em como tais produtos são",
    " desenvolvidos e poucas evidências empíricas sobre como atalhos no método podem ",
    "impactar nos achados das revisões. Enquanto programas de resposta rápida e seus ",
    "produtos são potencialmente um mecanismo importante para apoiar as políticas de ",
    "saúde informadas por evidências, essas estratégias ainda precisam ser testadas e",
    "mpiricamente.^ipt#Políticas informadas por evidências^ipt#Respostas rápidas^ipt#",
    "Revisão rápida^ipt#^dTechnology Assessment, Biomedical#^dEvidence-Informed Polic",
    "y#^dHealth Policy#2019-11-01T10:55:47#elcio.couto#2021-12-14T22:29:00#p#MEDICINA",
    " DE FAMILIA E COMUNIDADE#MEXICO#.. LATIN AMERICA AND THE CARIBBEAN#BRAZIL#.  ALL",
    " COUNTRIES#MEXICO#.. PAISES DE AMERICA LATINA#BRASIL#.  TODOS LOS PAISES#MÉXICO#",
    ".. PAISES DA AMERICA LATINA#BRASIL#.  TODOS OS PAISES#Mexique##Brésil##FI-ADMIN^",
    "i1024035^bLILACS#201600#0073#LILACS#SES-SP#PIE#FONTE#biblio-1024035#^uhttps://fi",
    "-admin.bvsalud.org/document/view/gewke^ipt^yPDF^qpdf##"
  ), c(
    # Real-world record with diacritics in the author field
    "01599000000000649000450000100080000000200150000800400070002300500020003000600030",
    "00320100079000350100087001140100079002010120014002800130010002940140007003040300",
    "03400311031000300345032000200348035001000350040000300360061003200363064001000395",
    "06500090040506700030041407100100041708700230042708700180045009100200046809200210",
    "04880930020005091130002005294400033005317210010005647210035005747210017006097220",
    "01000626722002800636722002000664723001000684723002800694723001900722725001000741",
    "72500010075172500010075277600260075385400070077985500050078690400070079190400160",
    "07989040008008149040009008229200008008319300008008390980006008477780015008530080",
    "08100868#AR338.1#biblio-1343902#SBARRA#S#as#Godoy, Ana Carolina^1Archivos de Med",
    "icina Familiar y General^cCórdoba^pAR^redt#Cacace, Patricio Jorge^1Archivos de M",
    "edicina Familiar y General^cBuenos Aires^pAR^redt#Santillán, María Valeria^1Arch",
    "ivos de Medicina Familiar y General^cTucumán^pAR#Editorial^ies#Editorial#^f1^l1#",
    "Archiv. med. fam. gen. (En línea)#16#1#2314-159X#es#Se dispone de una copia impr",
    "esa#mayo 2019#20190500#AR#Editorial#^dPeriodicals as Topic#^dFamily Practice#202",
    "1-11-16T12:06:44#fernanda.astigarraga#2021-11-25T10:10:57#p#MEDICINA DE FAMILIA ",
    "E COMUNIDADE#ARGENTINA#.. LATIN AMERICA AND THE CARIBBEAN#.  ALL COUNTRIES#ARGEN",
    "TINA#.. PAISES DE AMERICA LATINA#.  TODOS LOS PAISES#ARGENTINA#.. PAISES DA AMER",
    "ICA LATINA#.  TODOS OS PAISES#Argentine###FI-ADMIN^i1343902^bLILACS#201905#0032#",
    "LILACS#InstitutionalDB#BINACIS#UNISALUD#AR338.1#AR645.9#FONTE#biblio-1343902#^uh",
    "ttps://revista.famfyg.com.ar/index.php/AMFG/article/view/122/98^ies^yPDF^qpdf##"
  ), c(
    # Real-world record with a windows-1252 character which is invalid in ISO 8859-1
    "01549000000000625000450000100080000000200150000800500020002300600030002501001160",
    "00280120072001440130070002160140009002860300023002950310003003180320002003210350",
    "01000323040000300333064001000336065000900346067000300355071001700358072000200375",
    "07600220037707600230039907600240042208700220044608800270046808800080049509100200",
    "05030920013005230930020005361130002005564400010005587210006005687210035005747210",
    "01700609722000600626722002800632722002000660723000600680723002800686723001900714",
    "72500060073372500010073972500010074077600260074185400070076785500050077490400070",
    "0779920000800786930000500794098000600799778001500805008010300820#CL126.2#biblio-",
    "1152264#S#as#Rostion Allel, Carmen^3Departamento de Pediatría y Cirugía Infantil",
    "^2Facultad de Medicina^1Universidad de Chile^pCL#Educación en Chile (Desde el pr",
    "incipio…para entender la actualidad)^ies#Education in Chile (From the beginning ",
    "... to understand the present)#^l29^f24#Rev. pediatr. electrón#12#4#0718-0918#es",
    "#Dic. 2015#20151200#CL#Artigo Histórico#4#História do Século XV#História do Sécu",
    "lo XVI#História do Século XVII#^dEducaçăo^s/história#^dUniversidades^s/história#",
    "^dChile#2021-04-07T15:28:10#rodrigoacuna#2021-12-20T18:37:14#p#PEDIATRIA#CHILE#.",
    ". LATIN AMERICA AND THE CARIBBEAN#.  ALL COUNTRIES#CHILE#.. PAISES DE AMERICA LA",
    "TINA#.  TODOS LOS PAISES#CHILE#.. PAISES DA AMERICA LATINA#.  TODOS OS PAISES#Ch",
    "ili###FI-ADMIN^i1152264^bLILACS#201512#0073#LILACS#CL126.3#CL50#FONTE#biblio-115",
    "2264#^yPDF^uhttp://www.revistapediatria.cl/volumenes/2015/vol12num4/pdf/EDUCACIO",
    "N_MEDICA.pdf^qpdf^ies^gTrue##"
  ))
  unlist(record_samples) |>
    stringi::stri_write_lines(tmp_filename, "Windows-1252", "\r\n")

  d <- readLines(tmp_filename, ) |>
    stringi::stri_encode(from = "ISO-8859-1", to = "UTF-8")

  record_first <- read_record(d, current_line = 1)
  attr(record_first,"total_lines") <- NULL
  record_second <- read_record(d, 1 + length(record_samples[[1]]))
  attr(record_second,"total_lines") <- NULL
  record_third <- read_record(d, 1 + length(unlist(record_samples[1:2])))
  attr(record_third,"total_lines") <- NULL
  record_four <- read_record(d, 1 + length(unlist(record_samples[1:3])))
  attr(record_four,"total_lines") <- NULL

  stopifnot(identical(record_first, c(
    "044" = paste0("Methodology of plant eco-physiology: ",
                   "proceedings of the Montpellier Symposium"),
    "050" = "Incl. bibl.",
    "069" = paste0("Paper on: <plant physiology><plant transpiration>",
                   "<measurement and instruments>"),
    "024" = paste0("Techniques for the measurement of transpiration of ",
                   "individual plants"),
    "026" = "^aParis^bUnesco^c-1965",
    "030" = "^ap. 211-224^billus.",
    "070" = "Magalhaes, A.C.",
    "070" = "Franco, C.M."
  )))
  stopifnot(identical(
    record_second[c("002", "005", "006", "035", "065", "067")],
    c(`002` = "biblio-1024035",  # lilacs_id
      `005` = "S",               # lit_type; S = article
      `006` = "as",              # treat_level
      `035` = "1518-1812",       # issn
      `065` = "20160000",        # pub_date
      `067` = "BR")              # pub_country
  ))
  stopifnot(identical(
    record_third[c("002", "005", "006", "035", "065", "067")],
    c(`002` = "biblio-1343902",  # lilacs_id
      `005` = "S",               # lit_type; S = article
      `006` = "as",              # treat_level
      `035` = "2314-159X",       # issn
      `065` = "20190500",        # pub_date
      `067` = "AR")              # pub_country
  ))
  stopifnot(identical(
    record_four[c("002", "005", "006", "035", "065", "067")],
    c(`002` = "biblio-1152264",  # lilacs_id
      `005` = "S",               # lit_type; S = article
      `006` = "as",              # treat_level
      `035` = "0718-0918",       # issn
      `065` = "20151200",        # pub_date
      `067` = "CL")              # pub_country
  ))
  stopifnot(identical(
    record_second[names(record_second) %in% "030"],
    c("030" = "BIS, Bol. Inst. Saúde (Impr.)")
  ))
  stopifnot(identical(
    record_third[names(record_third) %in% "030"] ,
    c("030" = "Archiv. med. fam. gen. (En línea)")
  ))
  stopifnot(identical(
    record_four[names(record_four) %in% "030"] ,
    c("030" = "Rev. pediatr. electrón")
  ))

  # # Although abstracts (field 083) are not exported by read_record(),
  # # this is the reason this sample record was included in the unit
  # # test: windows-1252 character encoding.
  # #
  # stopifnot(
  #   # "incipio\u2026para entender la actualidad)^ies#Education in Chile (From the beginning "
  #   d[1 + length(unlist(record_samples[1:3])) + 10] |>
  #     stringi::stri_detect_fixed("\u2026")
  # )

})


# read_isis() ####

local({
  d <- read_isis(tmp_filename, progress = FALSE)
  stopifnot(
    all(vapply(d, is.matrix, NA)),
    all(vapply(d, is.character, NA))
  )
  stopifnot(
    !any(duplicated(d$articles[, "lilacs_id"])),
    all(d$articles[2:3, "lit_type"] == "S"),
    all(d$articles[2:3, "treat_level"] == "as")
  )
  stopifnot(
    all(d$authorships[, "lilacs_id"] %in% d$articles[, "lilacs_id"]),
    all(d$pubtypes[, "lilacs_id"] %in% d$articles[, "lilacs_id"])
  )
})

unlink(tmp_filename)
rm(tmp_filename)

# read_isis_title() ####

# We don't have good records to test this function, because the known
# documentation only provides examples for individual fields. Tests
# will be added if / when bugs are found.
