* Encoding: UTF-8.

T-TEST PAIRS=ManAngry WITH WomanAngry (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManApproachable WITH WomanApproachable (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManHappy WITH WomanHappy (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManWarm WITH WomanWarm (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManIntelligent WITH WomanIntelligent (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManAttractive WITH WomanAttractive (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManFriendly WITH WomanFriendly (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=ManCompetent WITH WomanCompetent (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

T-TEST PAIRS=MenMasc WITH WomanMasc (PAIRED)
  /CRITERIA=CI(.9500)
  /MISSING=ANALYSIS.

*Testing interactions with gender*

GLM ManAngry WomanAngry BY Gender
  /WSFACTOR=Angry 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(Angry) COMPARE ADJ(LSD)
  /EMMEANS=TABLES(Gender*Angry) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Angry 
  /DESIGN=Gender.

GLM ManApproachable WomanApproachable BY Gender
  /WSFACTOR=Approachable 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Approachable) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Approachable 
  /DESIGN=Gender.

GLM ManWarm WomanWarm BY Gender
  /WSFACTOR=Warm 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Warm) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Warm 
  /DESIGN=Gender.

GLM ManHappy WomanHappy BY Gender
  /WSFACTOR=Happy 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Happy) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Happy 
  /DESIGN=Gender.

GLM ManIntelligent WomanIntelligent BY Gender
  /WSFACTOR=Intelligent 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Intelligent) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Intelligent 
  /DESIGN=Gender.

GLM ManAttractive WomanAttractive BY Gender
  /WSFACTOR=Attractive 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Attractive) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Attractive 
  /DESIGN=Gender.

GLM ManFriendly WomanFriendly BY Gender
  /WSFACTOR=Friendly 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Friendly) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Friendly 
  /DESIGN=Gender.

GLM ManCompetent WomanCompetent BY Gender
  /WSFACTOR=Competent 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Competent) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Competent 
  /DESIGN=Gender.

GLM MenMasc WomanMasc BY Gender
  /WSFACTOR=Masc 2 Polynomial 
  /METHOD=SSTYPE(3)
  /EMMEANS=TABLES(Gender*Masc) 
  /PRINT=DESCRIPTIVE 
  /CRITERIA=ALPHA(.05)
  /WSDESIGN=Masc 
  /DESIGN=Gender.
