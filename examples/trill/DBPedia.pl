:-use_module(library(trill)).

:-trill.

/*
An extract of the DBPedia ontology, it contains structured information from Wikipedia.
http://dbpedia.org/
*/

/** <examples>

?- prob_sub_class('Place','PopulatedPlace',Prob).
?- sub_class('Place','PopulatedPlace',ListExpl).

*/

owl_rdf('<?xml version="1.0"?>

<!DOCTYPE rdf:RDF [
    <!ENTITY owl "http://www.w3.org/2002/07/owl#" >
    <!ENTITY Work "http://dbpedia.org/ontology/Work/" >
    <!ENTITY xsd "http://www.w3.org/2001/XMLSchema#" >
    <!ENTITY Lake "http://dbpedia.org/ontology/Lake/" >
    <!ENTITY Drug "http://dbpedia.org/ontology/Drug/" >
    <!ENTITY Canal "http://dbpedia.org/ontology/Canal/" >
    <!ENTITY Bridge "http://dbpedia.org/ontology/Bridge/" >
    <!ENTITY owl2xml "http://www.w3.org/2006/12/owl2-xml#" >
    <!ENTITY Stream "http://dbpedia.org/ontology/Stream/" >
    <!ENTITY School "http://dbpedia.org/ontology/School/" >
    <!ENTITY Weapon "http://dbpedia.org/ontology/Weapon/" >
    <!ENTITY Person "http://dbpedia.org/ontology/Person/" >
    <!ENTITY Rocket "http://dbpedia.org/ontology/Rocket/" >
    <!ENTITY Planet "http://dbpedia.org/ontology/Planet/" >
    <!ENTITY Software "http://dbpedia.org/ontology/Software/" >
    <!ENTITY Building "http://dbpedia.org/ontology/Building/" >
    <!ENTITY rdfs "http://www.w3.org/2000/01/rdf-schema#" >
    <!ENTITY Mountain "http://dbpedia.org/ontology/Mountain/" >
    <!ENTITY GrandPrix "http://dbpedia.org/ontology/GrandPrix/" >
    <!ENTITY Astronaut "http://dbpedia.org/ontology/Astronaut/" >
    <!ENTITY Automobile "http://dbpedia.org/ontology/Automobile/" >
    <!ENTITY Spacecraft "http://dbpedia.org/ontology/Spacecraft/" >
    <!ENTITY wgs84_pos "http://www.w3.org/2003/01/geo/wgs84_pos#" >
    <!ENTITY LunarCrater "http://dbpedia.org/ontology/LunarCrater/" >
    <!ENTITY SpaceShuttle "http://dbpedia.org/ontology/SpaceShuttle/" >
    <!ENTITY SpaceStation "http://dbpedia.org/ontology/SpaceStation/" >
    <!ENTITY SpaceMission "http://dbpedia.org/ontology/SpaceMission/" >
    <!ENTITY rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#" >
    <!ENTITY PopulatedPlace "http://dbpedia.org/ontology/PopulatedPlace/" >
    <!ENTITY Infrastructure "http://dbpedia.org/ontology/Infrastructure/" >
    <!ENTITY AutomobileEngine "http://dbpedia.org/ontology/AutomobileEngine/" >
    <!ENTITY ChemicalCompound "http://dbpedia.org/ontology/ChemicalCompound/" >
    <!ENTITY disponte "https://sites.google.com/a/unife.it/ml/disponte#" >
    <!ENTITY MeanOfTransportation "http://dbpedia.org/ontology/MeanOfTransportation/" >
    <!ENTITY GeopoliticalOrganisation "http://dbpedia.org/ontology/GeopoliticalOrganisation/" >
]>


<rdf:RDF xmlns="http://dbpedia.org/ontology/"
     xml:base="http://dbpedia.org/ontology/"
     xmlns:Software="http://dbpedia.org/ontology/Software/"
     xmlns:Astronaut="http://dbpedia.org/ontology/Astronaut/"
     xmlns:SpaceStation="http://dbpedia.org/ontology/SpaceStation/"
     xmlns:Building="http://dbpedia.org/ontology/Building/"
     xmlns:Bridge="http://dbpedia.org/ontology/Bridge/"
     xmlns:Work="http://dbpedia.org/ontology/Work/"
     xmlns:GrandPrix="http://dbpedia.org/ontology/GrandPrix/"
     xmlns:Spacecraft="http://dbpedia.org/ontology/Spacecraft/"
     xmlns:MeanOfTransportation="http://dbpedia.org/ontology/MeanOfTransportation/"
     xmlns:Infrastructure="http://dbpedia.org/ontology/Infrastructure/"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     xmlns:owl2xml="http://www.w3.org/2006/12/owl2-xml#"
     xmlns:PopulatedPlace="http://dbpedia.org/ontology/PopulatedPlace/"
     xmlns:Drug="http://dbpedia.org/ontology/Drug/"
     xmlns:ChemicalCompound="http://dbpedia.org/ontology/ChemicalCompound/"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:disponte="https://sites.google.com/a/unife.it/ml/disponte#"
     xmlns:SpaceShuttle="http://dbpedia.org/ontology/SpaceShuttle/"
     xmlns:Lake="http://dbpedia.org/ontology/Lake/"
     xmlns:LunarCrater="http://dbpedia.org/ontology/LunarCrater/"
     xmlns:School="http://dbpedia.org/ontology/School/"
     xmlns:Rocket="http://dbpedia.org/ontology/Rocket/"
     xmlns:wgs84_pos="http://www.w3.org/2003/01/geo/wgs84_pos#"
     xmlns:GeopoliticalOrganisation="http://dbpedia.org/ontology/GeopoliticalOrganisation/"
     xmlns:AutomobileEngine="http://dbpedia.org/ontology/AutomobileEngine/"
     xmlns:Automobile="http://dbpedia.org/ontology/Automobile/"
     xmlns:Canal="http://dbpedia.org/ontology/Canal/"
     xmlns:SpaceMission="http://dbpedia.org/ontology/SpaceMission/"
     xmlns:Planet="http://dbpedia.org/ontology/Planet/"
     xmlns:Stream="http://dbpedia.org/ontology/Stream/"
     xmlns:Weapon="http://dbpedia.org/ontology/Weapon/"
     xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
     xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:Person="http://dbpedia.org/ontology/Person/"
     xmlns:Mountain="http://dbpedia.org/ontology/Mountain/">
    <owl:Ontology rdf:about="http://dbpedia.org/ontology/">
        <owl:versionInfo xml:lang="en">Version 3.5</owl:versionInfo>
    </owl:Ontology>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Annotation properties
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- https://sites.google.com/a/unife.it/ml/disponte#probability -->

    <owl:AnnotationProperty rdf:about="&disponte;probability"/>
    


    <!-- 
    ///////////////////////////////////////////////////////////////////////////////////////
    //
    // Classes
    //
    ///////////////////////////////////////////////////////////////////////////////////////
     -->

    


    <!-- http://dbpedia.org/ontology/Actor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Actor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/AdministrativeRegion -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/AdministrativeRegion">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/AdultActor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/AdultActor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Airport -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Airport">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Album -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Album">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Ambassador -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Ambassador">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/AmericanFootballPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/AmericanFootballPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Architect -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Architect">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Artist -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Artist">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Astronaut -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Astronaut">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Athlete -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Athlete">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/BadmintonPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/BadmintonPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/BaseballPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/BaseballPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/BasketballPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/BasketballPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/BodyOfWater -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/BodyOfWater">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Book -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Book">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Boxer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Boxer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Bridge -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Bridge">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/BritishRoyalty -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/BritishRoyalty">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Building -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Building">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Canal -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Canal">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Cardinal -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Cardinal">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Cave -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Cave">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Chancellor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Chancellor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/ChristianBishop -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/ChristianBishop">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/City -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/City">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Cleric -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Cleric">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/CollegeCoach -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/CollegeCoach">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Comedian -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Comedian">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/ComicsCharacter -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/ComicsCharacter">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/ComicsCreator -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/ComicsCreator">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Congressman -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Congressman">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Continent -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Continent">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Country -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Country">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Cricketer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Cricketer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Criminal -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Criminal">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Cyclist -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Cyclist">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/EurovisionSongContestEntry -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/EurovisionSongContestEntry">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/FictionalCharacter -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/FictionalCharacter">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/FigureSkater -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/FigureSkater">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Film -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Film">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/FormulaOneRacer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/FormulaOneRacer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/GaelicGamesPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/GaelicGamesPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Governor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Governor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/GridironFootballPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/GridironFootballPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/HistoricPlace -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/HistoricPlace">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Hospital -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Hospital">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/IceHockeyPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/IceHockeyPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Island -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Island">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Journalist -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Journalist">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Judge -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Judge">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Lake -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Lake">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/LaunchPad -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/LaunchPad">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Lighthouse -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Lighthouse">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/LunarCrater -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/LunarCrater">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Magazine -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Magazine">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Mayor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Mayor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/MemberOfParliament -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/MemberOfParliament">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/MilitaryPerson -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/MilitaryPerson">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Model -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Model">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Monarch -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Monarch">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Monument -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Monument">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Mountain -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Mountain">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/MountainRange -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/MountainRange">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Musical -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Musical">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/MusicalArtist -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/MusicalArtist">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/MusicalWork -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/MusicalWork">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/NascarDriver -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/NascarDriver">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/NationalCollegiateAthleticAssociationAthlete -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/NationalCollegiateAthleticAssociationAthlete">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Newspaper -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Newspaper">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/OfficeHolder -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/OfficeHolder">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Park -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Park">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Person -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Person"/>
    


    <!-- http://dbpedia.org/ontology/Philosopher -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Philosopher">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Place -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Place">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.31</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Place"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.71</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Place"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.32</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Place"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/PlayboyPlaymate -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/PlayboyPlaymate">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/PokerPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/PokerPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Politician -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Politician">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Pope -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Pope">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/PopulatedPlace -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/PopulatedPlace">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/President -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/President">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/PrimeMinister -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/PrimeMinister">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/ProtectedArea -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/ProtectedArea">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/River -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/River">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/RugbyPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/RugbyPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Saint -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Saint">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Scientist -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Scientist">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Senator -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Senator">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Settlement -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Settlement">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.11</disponte:probability>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/Place"/>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.81</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.41</disponte:probability>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/ShoppingMall -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/ShoppingMall">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Single -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Single">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/SiteOfSpecialScientificInterest -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/SiteOfSpecialScientificInterest">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/SkiArea -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/SkiArea">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Skyscraper -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Skyscraper">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/SoccerManager -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/SoccerManager">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/SoccerPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/SoccerPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Software -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Software">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Song -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Song">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Stadium -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Stadium">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Station -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Station">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Stream -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Stream">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/TelevisionEpisode -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/TelevisionEpisode">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/TelevisionShow -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/TelevisionShow">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/TennisPlayer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/TennisPlayer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Town -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Town">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Valley -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Valley">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/VideoGame -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/VideoGame">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Work"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Village -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Village">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_A0_144_"/>
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.23</disponte:probability>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/PopulatedPlace"/>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.9</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.32</disponte:probability>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/Place"/>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.21</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A73_A0_144_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.3</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.91</disponte:probability>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/Settlement"/>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.67</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/Village"/>
        <owl:annotatedTarget rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
        <owl:annotatedProperty rdf:resource="&rdfs;subClassOf"/>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/VoiceActor -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/VoiceActor">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/WineRegion -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/WineRegion">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Work -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Work"/>
    


    <!-- http://dbpedia.org/ontology/WorldHeritageSite -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/WorldHeritageSite">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Place"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Wrestler -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Wrestler">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/Writer -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/Writer">
        <rdfs:subClassOf rdf:resource="http://dbpedia.org/ontology/Person"/>
    </owl:Class>
    


    <!-- http://dbpedia.org/ontology/A0_144_ -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/A0_144_">
        <owl:equivalentClass>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:equivalentClass>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.71</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/A0_144_"/>
        <owl:annotatedProperty rdf:resource="&owl;equivalentClass"/>
        <owl:annotatedTarget>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:annotatedTarget>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/A73_A0_ -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/A73_A0_">
        <owl:equivalentClass>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:equivalentClass>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.7</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/A73_A0_"/>
        <owl:annotatedProperty rdf:resource="&owl;equivalentClass"/>
        <owl:annotatedTarget>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:annotatedTarget>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/A73_A0_144_ -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/A73_A0_144_">
        <owl:equivalentClass>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:equivalentClass>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.81</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/A73_A0_144_"/>
        <owl:annotatedProperty rdf:resource="&owl;equivalentClass"/>
        <owl:annotatedTarget>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/PopulatedPlace"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:annotatedTarget>
    </owl:Axiom>
    


    <!-- http://dbpedia.org/ontology/A73_144_ -->

    <owl:Class rdf:about="http://dbpedia.org/ontology/A73_144_">
        <owl:equivalentClass>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:equivalentClass>
    </owl:Class>
    <owl:Axiom>
        <disponte:probability rdf:datatype="&xsd;decimal">0.51</disponte:probability>
        <owl:annotatedSource rdf:resource="http://dbpedia.org/ontology/A73_144_"/>
        <owl:annotatedProperty rdf:resource="&owl;equivalentClass"/>
        <owl:annotatedTarget>
            <owl:Class>
                <owl:intersectionOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Place"/>
                    <rdf:Description rdf:about="http://dbpedia.org/ontology/Settlement"/>
                </owl:intersectionOf>
            </owl:Class>
        </owl:annotatedTarget>
    </owl:Axiom>
</rdf:RDF>



<!-- Generated by the OWL API (version 3.4.2) http://owlapi.sourceforge.net -->
').
