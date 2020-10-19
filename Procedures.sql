-- Groupe 1				manque hibernation et une procedure a revoir
CREATE OR REPLACE PROCEDURE MAJ_Nourriture_Volaille AS											-- Checked (juste pas d'hibernation)
	boire volaille.ABREUVAGE_V%type;
	manger volaille.NOURRI_V%type;
	mort boolean;
	nbJourJeune volaille.NB_JEUNE_V%type;
	poids volaille.POIDS_V%type;
BEGIN
	for r in (SELECT ID_VOLAILLE, ABREUVAGE_V, NOURRI_V, NB_JEUNE_V, POIDS_V FROM volaille WHERE hibernatin = 'F') loop
		boire := r.ABREUVAGE_V;
		manger := r.NOURRI_V;
		poids := r.POIDS_V;
		mort := FALSE;

		if (r.NB_JEUNE_V >= 0) then
			nbJourJeune := r.NB_JEUNE_V;
		else
			nbJourJeune := 0;
		end if;

		if ((manger = 'T') and (boire = 'T')) then
			nbJourJeune := 0;
			poids := poids + 0.65;
		elsif (manger = 'T') then
			nbJourJeune := 0;
			poids := poids + 0.5;
		else
			if (nbJourJeune = 0) then
				poids := poids - 0.2;
			elsif (nbJourJeune = 1) then
				poids := poids - 0.5;
			elsif (nbJourJeune = 2) then
				poids := poids - 1;
			elsif (nbJourJeune = 3) then
				mort := TRUE;
			end if;
			nbJourJeune := nbJourJeune + 1;
		end if;

		-- dans tous les cas, manger et boire deviennent faux...
		manger := 'F';
		boire := 'F';

		-- verification final du poids
		if (poids <= 0) then
			mort := TRUE;
		elsif (poids > 3.5) then
			poids := 3.5;
		end if;

		-- MaJ Database
		if (mort) then
			DELETE FROM volaille WHERE ID_VOLAILLE = r.ID_VOLAILLE;
		else
			UPDATE volaille SET ABREUVAGE_V = boire, NOURRI_V = manger, NB_JEUNE_V = nbJourJeune, POIDS_V = poids WHERE ID_VOLAILLE = r.ID_VOLAILLE;
		end if;
	end loop;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE MAJ_Nourriture_Vache AS															-- Checked
	boire ferme.ABREUVAGE_VACHE%type;
	manger ferme.NOURRI_VACHE%type;
	poids ferme.POIDS_VACHE%type;
BEGIN
	-- La vache ne peut pas mourrir dans cette procedure, car toujours nourrie (par l'herbe)
	for r in (SELECT ID_FERMIER, ABREUVAGE_VACHE, NOURRI_VACHE, POIDS_VACHE FROM ferme WHERE HIBERNATION = 'F') loop
		boire := r.ABREUVAGE_VACHE;
		manger := r.NOURRI_VACHE;
		poids := r.POIDS_VACHE;

		poids := poids + 5; -- poids du a l herbe (dans tous les cas)
		if (manger = 'T') then -- a mange de la paille
			poids := poids + 3;
		end if;
		if (boire = 'T') then -- a bu => prise de poids dans tous les cas car a manger de l herbe (donc a manger)
			poids := poids + 1;
		end if;

		-- dans tous les cas, manger et boire deviennent faux...
		manger := 'F';
		boire := 'F';

		-- verification final du poids
		if (poids > 750) then
			poids := 750;
		end if;

		-- MaJ Database
		UPDATE ferme SET ABREUVAGE_VACHE = boire, NOURRI_VACHE = manger, NB_JEUNE_VACHE = 0, POIDS_VACHE = poids WHERE ID_FERMIER = r.ID_FERMIER;
	end loop;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE MAJ_Nourriture_Lapin AS															-- Checked
	boireLapereau ferme.ABREUVAGE_J%type;
	mangerLapereau ferme.NOURRI_J%type;
	boireLapin ferme.ABREUVAGE_L%type;
	mangerLapin ferme.NOURRI_L%type;
	nbPetit ferme.NB_PETIT%type;
	nbMoyen ferme.NB_MOYEN%type;
	nbGros ferme.NB_GROS%type;
	nbMale ferme.NB_MALE%type;
	nbFemelle ferme.NB_FEMELLE%type;
	nbLapereauTotal integer;
	nbLapereauDevenantMal integer;
	nbLapereauMourrant integer;
BEGIN
	for r in (SELECT ID_FERMIER, NB_MALE, NB_FEMELLE, NB_PETIT, NB_MOYEN, NB_GROS, ABREUVAGE_L, NOURRI_L, ABREUVAGE_J, NOURRI_J FROM ferme WHERE HIBERNATION = 'F') loop
		boireLapereau := r.ABREUVAGE_J;
		mangerLapereau := r.NOURRI_J;
		boireLapin := r.ABREUVAGE_L;
		mangerLapin := r.NOURRI_L;
		nbPetit := r.NB_PETIT;
		nbMoyen := r.NB_MOYEN;
		nbGros := r.NB_GROS;
		nbMale := r.NB_MALE;
		nbFemelle := r.NB_FEMELLE;
		nbLapereauTotal := nbGros + nbMoyen + nbPetit;

		-- les lapins
		if (mangerLapin = 'F') then
			-- pas nourris => on tue une parties des lapins males (1 au minimun)
			nbMale := nbMale - nbMale * dbms_random.value - 1;
			nbFemelle := nbFemelle - nbFemelle * dbms_random.value - 1;
		end if;

		-- les lapereaux
		if ((mangerLapereau = 'T') and (boireLapin = 'T')) then
			-- les gros deviennent adultes
			nbLapereauDevenantMal := nbGros * dbms_random.value;
			nbMale := nbMale + nbLapereauDevenantMal;
			nbFemelle := nbFemelle + nbGros - nbLapereauDevenantMal;
			-- les moyens deviennent gros
			nbGros := nbMoyen;
			-- les petits deviennent moyens
			nbMoyen := nbPetit;
			nbPetit := 0;
		elsif (mangerLapereau = 'F') then
			-- des lapereaux meurent (les petits en premiers )
			nbLapereauMourrant := nbLapereauTotal * dbms_random.value;
			nbPetit := nbPetit - nbLapereauMourrant;
			nbLapereauMourrant := nbLapereauMourrant - nbPetit;
			if (nbLapereauMourrant > 0) then
				nbMoyen := nbMoyen - nbLapereauMourrant;
				nbLapereauMourrant := nbLapereauMourrant - nbMoyen;
				if (nbLapereauMourrant > 0) then
					nbGros := nbGros - nbLapereauMourrant;
					nbLapereauMourrant := nbLapereauMourrant - nbGros;
				end if;
			end if;
		end if;

		-- dans tous les cas, manger et boire deviennent faux...
		boireLapereau := 'F';
		mangerLapereau := 'F';
		boireLapin := 'F';
		mangerLapin := 'F';

		-- verification final des quantites
		if (nbPetit < 0) then
			nbPetit := 0;
		end if;
		if (nbMoyen < 0) then
			nbMoyen := 0;
		end if;
		if (nbGros < 0) then
			nbGros := 0;
		end if;
		if (nbMale < 0) then
			nbMale := 0;
		end if;
		if (nbFemelle < 0) then
			nbFemelle := 0;
		end if;

		-- mise a jour de la base
		UPDATE ferme SET NB_MALE = nbMale, NB_FEMELLE = nbFemelle, NB_PETIT = nbPetit, NB_MOYEN = nbMoyen, NB_GROS = nbGros, ABREUVAGE_L = boireLapin, NOURRI_L = mangerLapin, ABREUVAGE_J = boireLapereau, NOURRI_J = mangerLapereau WHERE ID_FERMIER = r.ID_FERMIER;
	end loop;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE MAJ_Nourriture AS																-- Checked
BEGIN
	MAJ_Nourriture_Volaille;
	MAJ_Nourriture_Vache;
	MAJ_Nourriture_Lapin;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE Changement_d_age AS														-- !!! Deja fait ds nourrire
    nb_sexe_masculin ferme.nb_male%type;
    nb_nouveaux_adulte ferme.nb_gros%type;
    CURSOR clapier IS
		SELECT id_fermier,nb_male,nb_femelle,nb_gros,abreuvage_j,nourri_j
		FROM ferme
		WHERE HIBERNATION = 'F';
    lapins clapier%rowtype;
BEGIN
	-- MAJ de l'age des volailles
	-- les volailles prenent un jour quel que soit leur etat de sante et leur nutrition
	UPDATE volaille SET age_v = age_v + 1 ;

	-- MAJ de l'age des laperaux
	-- les laperaux prenent un jour seulement si ils ont ete nourris et abreuves, peu importe leur etat de sante
	-- le clapier ne peut contenir plus de 50 individus
	-- la determination du sexe d'un laperau se fait a son passage a l'age adulte par un random
	OPEN clapier ;
	LOOP
	EXIT WHEN clapier%NOTFOUND ;
		FETCH clapier INTO lapins ;
		IF lapins.abreuvage_j='T' and lapins.nourri_j='T'
		THEN
			IF lapins.nb_male + lapins.nb_femelle + lapins.nb_gros <= 50
			THEN
				nb_nouveaux_adulte := lapins.nb_gros ;
			ELSE
				nb_nouveaux_adulte := 50 - (lapins.nb_male + lapins.nb_femelle) ;
			END IF ;
			nb_sexe_masculin := dbms_random.value * nb_nouveaux_adulte ;
			UPDATE ferme SET nb_male = lapins.nb_male + nb_sexe_masculin WHERE id_fermier=lapins.id_fermier ;
			UPDATE ferme SET nb_femelle = lapins.nb_femelle + (nb_nouveaux_adulte - nb_sexe_masculin) WHERE id_fermier=lapins.id_fermier ;
		END IF ;
	END LOOP ;
	CLOSE clapier ;
	UPDATE ferme SET nb_gros = nb_moyen WHERE abreuvage_j='T' and nourri_j='T' ;
	UPDATE ferme SET nb_moyen = nb_petit WHERE abreuvage_j='T' and nourri_j='T' ;
	-- la naissance des plus petits laperaux n'est pas geree ici
	UPDATE ferme SET nb_petit = 0 WHERE abreuvage_j='T' and nourri_j='T' ;

	-- MAJ de l'age de la vache
	-- la vache prend un jour quel que soit son etat de sante et sa nutrition
	-- lorsqu'une vache meurt, tous les champs la concernant sont passes a NULL
	UPDATE ferme SET age_vache = age_vache + 1 WHERE age_vache IS NOT NULL ;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE score AS																		-- Checked
	nbre_oeufs_total_vendu      integer ;
	nbre_oeufs_vendu            integer ;
	nbre_laits_total_vendu      integer ;
	nbre_laits_vendu            integer ;
	nbre_lapins_total_vendu     integer ;
	nbre_lapins_vendu           integer ;
	nbre_achat_eleveur          integer ;
	nbre_achat_eleveur_total    integer ;
	nbre_vente_eleveur          integer ;
	nbre_vente_eleveur_total    integer ;
	solde                       integer ;
	solde_total                 integer ;
	nbre_gens_total             integer ;
	score_                      integer ;
	classements                 integer ;
BEGIN
	-- Initialisation des varibles globales :
	SELECT COUNT(NB_VENDU) into nbre_oeufs_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_oeuf ;
	if (nbre_oeufs_total_vendu != 0) then
		SELECT SUM(NB_VENDU) into nbre_oeufs_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_oeuf ;
	else
		nbre_oeufs_total_vendu := 1;
	end if ;

	SELECT COUNT(NB_VENDU) into nbre_laits_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lait ;
	if (nbre_laits_total_vendu != 0) then
		SELECT SUM(NB_VENDU) into nbre_laits_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lait ;
	else
		nbre_laits_total_vendu := 1;
	end if ;

	SELECT COUNT(NB_VENDU) into nbre_lapins_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle;
	if (nbre_lapins_total_vendu != 0) then
		SELECT SUM(NB_VENDU) into nbre_lapins_total_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle;
	else
		nbre_lapins_total_vendu := 1;
	end if ;

	SELECT COUNT(NB_VENDU) into nbre_vente_eleveur_total FROM a_vendu WHERE not(ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle or ID_ARTICLE = jmeumeu.id_lait or ID_ARTICLE = jmeumeu.id_oeuf) ;
	if (nbre_vente_eleveur_total != 0) then
		SELECT SUM(NB_VENDU) into nbre_vente_eleveur_total FROM a_vendu WHERE not(ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle or ID_ARTICLE = jmeumeu.id_lait or ID_ARTICLE = jmeumeu.id_oeuf) ;
	else
		nbre_vente_eleveur_total := 1 ;
	end if ;

	SELECT SUM(ECUS) into solde_total FROM ferme ;
	SELECT SUM(NB_ACHATS_JOUR) into nbre_achat_eleveur_total FROM ferme WHERE HIBERNATION = 'F';
	SELECT COUNT(*) into nbre_gens_total FROM ferme ;

	for un_gens in (SELECT ID_FERMIER, ECUS, NB_ACHATS_JOUR FROM ferme WHERE hibernation = 'F') loop
		-- Initialisation des varibles :
		solde                := un_gens.ECUS             ;
		nbre_achat_eleveur   := un_gens.NB_ACHATS_JOUR   ;

		SELECT COUNT(NB_VENDU) into nbre_oeufs_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_oeuf and ID_FERMIER = un_gens.ID_FERMIER ;
		if (nbre_oeufs_vendu != 0) then
			SELECT SUM(NB_VENDU) into nbre_oeufs_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_oeuf and ID_FERMIER = un_gens.ID_FERMIER ;
		end if ;

		SELECT COUNT(NB_VENDU) into nbre_laits_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lait and ID_FERMIER = un_gens.ID_FERMIER ;
		if (nbre_laits_vendu != 0) then
			SELECT SUM(NB_VENDU) into nbre_laits_vendu FROM a_vendu WHERE ID_ARTICLE = jmeumeu.id_lait and ID_FERMIER = un_gens.ID_FERMIER ;
		end if ;

		SELECT COUNT(NB_VENDU) into nbre_lapins_vendu FROM a_vendu WHERE (ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle) and ID_FERMIER = un_gens.ID_FERMIER ;
		if (nbre_lapins_vendu != 0) then
			SELECT SUM(NB_VENDU) into nbre_lapins_vendu FROM a_vendu WHERE (ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle) and ID_FERMIER = un_gens.ID_FERMIER ;
		end if ;

		SELECT COUNT(NB_VENDU) into nbre_vente_eleveur FROM a_vendu WHERE ID_FERMIER = un_gens.ID_FERMIER and not(ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle or ID_ARTICLE = jmeumeu.id_lait or ID_ARTICLE = jmeumeu.id_oeuf) ;
		if (nbre_vente_eleveur != 0) then
			SELECT SUM(NB_VENDU) into nbre_vente_eleveur FROM a_vendu WHERE ID_FERMIER = un_gens.ID_FERMIER and not(ID_ARTICLE = jmeumeu.id_lapin_male or ID_ARTICLE = jmeumeu.id_lapin_femelle or ID_ARTICLE = jmeumeu.id_lait or ID_ARTICLE = jmeumeu.id_oeuf) ;
		end if ;

		score_ := (jmeumeu.coeff_achat*(nbre_achat_eleveur/nbre_achat_eleveur_total) + jmeumeu.coeff_vente*(nbre_vente_eleveur/nbre_vente_eleveur_total) + jmeumeu.coeff_oeuf*(nbre_oeufs_vendu/nbre_oeufs_total_vendu) + jmeumeu.coeff_lapin*(nbre_lapins_vendu/nbre_lapins_total_vendu) + jmeumeu.coeff_lait*(nbre_laits_vendu/nbre_laits_total_vendu) + jmeumeu.coeff_solde*(solde/solde_total) )*nbre_gens_total ;

		UPDATE ferme set score=score_ WHERE id_fermier = un_gens.id_fermier;
	end loop ;
	classements := 1 ;
	for un_gens in (SELECT ID_FERMIER, SCORE FROM ferme ORDER BY SCORE DESC) loop
		UPDATE ferme set CLASSEMENT = classements WHERE ID_FERMIER = un_gens.ID_FERMIER;
		classements := classements + 1 ;
	end loop ;
	COMMIT;
END;
/




CREATE OR REPLACE PROCEDURE MAJ_MALADIE AS																	-- Checked
	--curseur sur les fermiers
	CURSOR FARM IS
		SELECT *
		FROM FERME
		WHERE HIBERNATION = 'F'
		FOR UPDATE;
	--curseur parametre sur les volailles
	CURSOR MALADIE_V(FERMIER FERME.ID_FERMIER%TYPE) IS
		SELECT V.NB_MALADE_V, V.ID_VOLAILLE
		FROM VOLAILLE V,FERME F
		WHERE F.ID_FERMIER = V.ID_FERMIER
		AND V.ID_FERMIER = FERMIER
		FOR UPDATE;
	SEUIL       FLOAT;
BEGIN
	--initialisation
	SEUIL := 0.6;

	--Parcours des fermes pour les morts...
	FOR UNE_FERME IN FARM
	LOOP
		--mort des volailles
        FOR UNE_VOLAILLE IN MALADIE_V(UNE_FERME.ID_FERMIER)
        LOOP
			IF (UNE_VOLAILLE.NB_MALADE_V = 4)
			THEN
				DELETE FROM VOLAILLE WHERE CURRENT OF MALADIE_V;
			END IF;
        END LOOP;

        --mort des vaches
        IF (UNE_FERME.NB_MALADE_VACHE = 4)
		THEN
			UPDATE FERME SET POIDS_VACHE = 0 WHERE CURRENT OF FARM ;
            UPDATE FERME SET NB_MALADE_VACHE = 0 WHERE CURRENT OF FARM;
            UPDATE FERME SET SALE_VACHE = 'F' WHERE CURRENT OF FARM;
		END IF;

		--mort des lapins
		IF (UNE_FERME.NB_MALADE_L = 1)
		THEN
            UPDATE FERME SET NB_MALE = NB_MALE - CAST((NB_MALE * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
            UPDATE FERME SET NB_FEMELLE = NB_FEMELLE - CAST((NB_FEMELLE * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;

			IF (UNE_FERME.NB_MALE = 0 AND UNE_FERME.NB_FEMELLE = 0 )
			THEN
				UPDATE FERME SET NB_MALE = 0 WHERE CURRENT OF FARM;
				UPDATE FERME SET NB_FEMELLE = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_MALADE_L = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET SALE_L = 'F' WHERE CURRENT OF FARM;
			END IF;
		END IF;

		--mort des lapereaux
		IF (UNE_FERME.NB_MALADE_J = 1)
		THEN
			UPDATE FERME SET NB_GROS = NB_GROS - CAST((NB_GROS * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
			UPDATE FERME SET NB_MOYEN = NB_MOYEN - CAST((NB_MOYEN * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
			UPDATE FERME SET NB_PETIT = NB_PETIT - CAST((NB_PETIT * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;

			IF (UNE_FERME.NB_GROS = 0 AND UNE_FERME.NB_MOYEN = 0 AND UNE_FERME.NB_PETIT = 0)
			THEN
				UPDATE FERME SET NB_GROS = 0 WHERE CURRENT OF FARM;
				UPDATE FERME SET NB_MOYEN = 0 WHERE CURRENT OF FARM;
				UPDATE FERME SET NB_PETIT = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_MALADE_J = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET SALE_J = 'F' WHERE CURRENT OF FARM;
			END IF;
		END IF;
	END LOOP;

	--Parcours des fermes pour la mise a jour des maladies
	FOR UNE_FERME IN FARM
	LOOP
		--mise a jour des volailles
		FOR UNE_VOLAILLE IN MALADIE_V(UNE_FERME.ID_FERMIER)
		LOOP
			IF (UNE_VOLAILLE.NB_MALADE_V = 0)
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE VOLAILLE SET NB_MALADE_V = 1 WHERE CURRENT OF MALADIE_V;
				END IF;
			ELSE
				UPDATE VOLAILLE SET NB_MALADE_V = NB_MALADE_V + 1 WHERE CURRENT OF MALADIE_V;
           END IF;
		END LOOP;

		--mise a jour des vaches
		IF (UNE_FERME.POIDS_VACHE > 0)
		THEN
			IF (UNE_FERME.NB_MALADE_VACHE = 0)
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET NB_MALADE_VACHE = 1 WHERE CURRENT OF FARM;
				END IF;
			ELSE
				UPDATE FERME SET NB_MALADE_VACHE = NB_MALADE_VACHE + 1 WHERE CURRENT OF FARM;
			END IF;
		END IF;

		--mise a jour des lapins
		IF (UNE_FERME.NB_MALE > 0 OR UNE_FERME.NB_FEMELLE > 0)
		THEN
			IF (UNE_FERME.NB_MALADE_L = 0)
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET NB_MALADE_L = 1 WHERE CURRENT OF FARM;
				END IF;
			END IF;
		END IF;

		--mise a jour des lapereaux
		IF (UNE_FERME.NB_GROS > 0 OR UNE_FERME.NB_MOYEN > 0 OR UNE_FERME.NB_PETIT > 0)
		THEN
			IF (UNE_FERME.NB_MALADE_J = 0)
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET NB_MALADE_J = 1 WHERE CURRENT OF FARM;
				END IF;
			END IF;
		END IF;
	END LOOP;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE MAJ_SALE AS																		-- Checked
	--curseur sur les fermiers
	CURSOR FARM IS
		SELECT *
		FROM FERME
		WHERE HIBERNATION = 'F'
		FOR UPDATE;
	--curseur parametre sur les volailles
	CURSOR SALE_VOL(FERMIER FERME.ID_FERMIER%TYPE) IS
	    SELECT V.NB_MALADE_V
		FROM VOLAILLE V,FERME F
		WHERE F.ID_FERMIER = V.ID_FERMIER
		AND V.ID_FERMIER = FERMIER
		AND V.SALE_V = 'F'
		FOR UPDATE;
	SEUIL FLOAT;
BEGIN
	--initialisation
	SEUIL := 0.4;
	FOR UNE_FERME IN FARM
	LOOP
		--Mort des lapins et laperaux
		--mort des lapins
		IF (UNE_FERME.SALE_L = 'T')
		THEN
			UPDATE FERME SET NB_MALE = NB_MALE - CAST((NB_MALE * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
            UPDATE FERME SET NB_FEMELLE = NB_FEMELLE - CAST((NB_FEMELLE * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;

			IF (UNE_FERME.NB_MALE = 0 AND UNE_FERME.NB_FEMELLE = 0)
			THEN
				UPDATE FERME SET NB_MALE = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_FEMELLE = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_MALADE_L = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET SALE_L = 'F' WHERE CURRENT OF FARM;
			END IF;
		END IF;

		--mort des lapereaux
		IF (UNE_FERME.SALE_J = 'T')
		THEN
			UPDATE FERME SET NB_GROS = NB_GROS - CAST((NB_GROS * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
			UPDATE FERME SET NB_MOYEN = NB_MOYEN - CAST((NB_MOYEN * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;
			UPDATE FERME SET NB_PETIT = NB_PETIT - CAST((NB_PETIT * dbms_random.value) AS INTEGER) WHERE CURRENT OF FARM;

			IF (UNE_FERME.NB_GROS = 0 AND UNE_FERME.NB_MOYEN = 0 AND UNE_FERME.NB_PETIT = 0)
			THEN
				UPDATE FERME SET NB_GROS = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_MOYEN = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_PETIT = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET NB_MALADE_J = 0 WHERE CURRENT OF FARM;
                UPDATE FERME SET SALE_J = 'F' WHERE CURRENT OF FARM;
			END IF;
		END IF;

		--mise a jour des volailles
		FOR UNE_VOLAILLE IN SALE_VOL(UNE_FERME.ID_FERMIER)
		LOOP
            IF (dbms_random.value>SEUIL)
            THEN
				UPDATE VOLAILLE SET SALE_V = 'T' WHERE CURRENT OF SALE_VOL;
            END IF;
		END LOOP;

		--mise a jour des vaches
		IF (UNE_FERME.POIDS_VACHE > 0)
		THEN
			IF (UNE_FERME.SALE_VACHE = 'F')
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET SALE_VACHE = 'T' WHERE CURRENT OF FARM;
				END IF;
			END IF;
		END IF;

		--mise a jour des lapins
		IF (UNE_FERME.NB_MALE > 0 OR UNE_FERME.NB_FEMELLE > 0)
	    THEN
			IF (UNE_FERME.SALE_L = 'F')
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET SALE_L = 'T' WHERE ID_FERMIER LIKE UNE_FERME.ID_FERMIER;
				END IF;
			END IF;
		END IF;

		--mise a jour des lapereaux
		IF (UNE_FERME.NB_GROS > 0 OR UNE_FERME.NB_MOYEN > 0 OR UNE_FERME.NB_PETIT > 0)
		THEN
			IF (UNE_FERME.SALE_J = 'F')
			THEN
				IF (dbms_random.value>SEUIL)
				THEN
					UPDATE FERME SET SALE_J = 'T' WHERE CURRENT OF FARM;
				END IF;
			END IF;
        END IF;
	END LOOP;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE MAJ_GLOBALE AS																	-- Checked
	CURSOR FERMIERS IS
		SELECT ID_FERMIER
		FROM FERME
		WHERE HIBERNATION = 'F'
		FOR UPDATE;
	CURSOR VOLAILLES_DE(FERMIER FERME.ID_FERMIER%TYPE) IS
		SELECT ID_VOLAILLE
		FROM VOLAILLE
		WHERE ID_FERMIER = FERMIER
		FOR UPDATE;
BEGIN
	FOR FERMIER IN FERMIERS LOOP
		FOR VOLAILLE IN VOLAILLES_DE(FERMIER.ID_FERMIER) LOOP
			Creation_oeuf(VOLAILLE.ID_VOLAILLE,FERMIER.ID_FERMIER);  	-- Ponte des poules
		END LOOP;
		Creation_lait(FERMIER.ID_FERMIER);   							-- Remplissage du pis des vaches
	END LOOP;

	MAJ_Nourriture();     -- Mise à jour des animaux nourris
	MAJ_MALADIE();        -- Mise à jour des animaux malades
	MAJ_SALE();           -- Mise à jour des animaux sales
	Changement_d_age();   -- Changement d'âge des animaux si besoin est
	score();              -- Mise à jour des scores
	Naissance_Lapins();   -- Naissance des jolis petits lapinous
	Mise_A_JourCoop();    -- Remplissage de la coopérative
	Hiberner();			  -- Hibernation automatique au bout de 3 jours
END;
/





CREATE OR REPLACE PROCEDURE Mise_A_JourCoop AS																-- Checked
		nb_fermier INTEGER ;
		facteur_mul INTEGER ;
BEGIN
	-- la cooperative possede un stock adapte au nombre de fermier
	SELECT count(*) INTO nb_fermier FROM ferme ;
	-- on augmente d'une quantite tous les 20 fermiers
	facteur_mul := nb_fermier / 20 + 1 ;
	-- reinitialisation du nombre de sac de nourriture
	UPDATE article SET quantite = jmeumeu.qte_sac_de_graine * facteur_mul WHERE id_article = jmeumeu.id_sac_de_graine ;
	-- reinitialisation du nombre de seau d'eau
	UPDATE article SET quantite = jmeumeu.qte_seau_d_eau * facteur_mul WHERE id_article = jmeumeu.id_seau_d_eau ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_seringue * facteur_mul WHERE id_article = jmeumeu.id_seringue ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_botte_de_paille * facteur_mul WHERE id_article = jmeumeu.id_botte_de_paille ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_poussin_male * facteur_mul WHERE id_article = jmeumeu.id_poussin_male ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_poussin_femelle * facteur_mul WHERE id_article = jmeumeu.id_poussin_femelle ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_coq * facteur_mul WHERE id_article = jmeumeu.id_coq ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_poule * facteur_mul WHERE id_article = jmeumeu.id_poule ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_lapin_male * facteur_mul WHERE id_article = jmeumeu.id_lapin_male ;
	-- reinitialisation du nombre de
	UPDATE article SET quantite = jmeumeu.qte_lapin_femelle * facteur_mul WHERE id_article = jmeumeu.id_lapin_femelle ;
	COMMIT;
END;
/





























-- Groupe 2				CHECKED
CREATE OR REPLACE PROCEDURE Ajouter_a_collection															-- Checked
(
	fermier ENTREPOSE.ID_FERMIER%type,
	id ENTREPOSE.ID_ARTICLE%type,
	nb ENTREPOSE.QUANTITE%type
) AS
	boolCollect ARTICLE.collectionnable%TYPE;
BEGIN
	SELECT collectionnable into boolCollect FROM ARTICLE WHERE id_article = id;
   	IF (boolCollect = 'T')
	THEN
      	INSERT INTO ENTREPOSE(ENTREPOSE.id_fermier,ENTREPOSE.id_article,ENTREPOSE.quantite,ENTREPOSE.collection) VALUES(fermier,id,nb,'T');
   	ELSE
      	RAISE_APPLICATION_ERROR(-20112,'Article non collectionnable');
	END IF;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE Vente_article_remise															-- Checked
(
	fermier MARCHE_VEND.ID_FERMIER%type,
	id ARTICLE.ID_ARTICLE%type,
	nb MARCHE_VEND.NB_VENTE%type,
	prix MARCHE_VEND.prix%type
) AS
	dispo ENTREPOSE.quantite%type;
	nb_rest ENTREPOSE.quantite%type;
BEGIN
	SELECT quantite INTO dispo FROM ENTREPOSE WHERE ENTREPOSE.id_article = id;
	IF dispo >= nb
	THEN
      	nb_rest := dispo - nb ;
	    UPDATE MARCHE_VEND set MARCHE_VEND.nb_vente = nb_rest WHERE id = MARCHE_VEND.id_article ;
		INSERT INTO MARCHE_VEND(MARCHE_VEND.id_fermier,MARCHE_VEND.id_article,MARCHE_VEND.nb_vente,MARCHE_VEND.prix) VALUES(fermier,id,nb,prix);
	ELSE
		RAISE_APPLICATION_ERROR(-20113,'Trop d`articles vendus');
	END IF;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE Acheter_articleM																-- Checked
(
	id_fer FERME.ID_FERMIER%type,
	id_art ARTICLE.ID_ARTICLE%type,
	quant INTEGER
) AS
	fermier FERME%rowtype;
	prod A_VENDU%rowtype;
	prod_E ENTREPOSE%rowtype;
	produit MARCHE_VEND%rowtype;
	produit_M MARCHE_VEND%rowtype;
	nb_achat FERME.NB_ACHATS_JOUR%type;
	age VOLAILLE.AGE_V%type;
	prix ARTICLE.PRIX_ACHAT%type;
	vente integer := 0;
	tmp integer;
	droit boolean;
	fait integer;
	cursor prods IS
		SELECT * FROM A_VENDU WHERE id_fer = A_VENDU.ID_FERMIER;
	cursor produits IS
		SELECT * FROM MARCHE_VEND WHERE id_art = MARCHE_VEND.ID_ARTICLE;
BEGIN
	SELECT * INTO fermier FROM FERME WHERE id_fer = FERME.ID_FERMIER;
	droit := FALSE;
    IF(fermier.NB_ACHATS_JOUR < 1)
	THEN
       	IF(fermier.NB_ACHATS_JOUR + quant <= 12)
		THEN
       		droit := TRUE;
        END IF;
    END IF;
	-- si l'article est mis en vente au marché avec la quantité demandée
   	SELECT SUM(NB_VENTE) INTO vente FROM MARCHE_VEND WHERE id_art = MARCHE_VEND.ID_ARTICLE;
   	IF (vente >= quant)
	THEN
		-- Ajouter
      	IF(droit)
      	THEN
        	-- Si le fermier a assez d'argent pour acheter
         	SELECT PRIX_ACHAT INTO prix FROM ARTICLE WHERE id_art = ARTICLE.ID_ARTICLE;
         	IF(fermier.ECUS >= quant*prix)
			THEN
         	-- Recherche du fermier qui vent ses produits avec le prix le moins cher
            	WHILE (fait<quant) LOOP
               		OPEN produits;
               		FETCH produits INTO produit_M;
               		LOOP
                  		FETCH produits INTO produit;
                  		IF (produit.PRIX < produit_M.PRIX)
						THEN
                     		produit_M.ID_FERMIER := produit.ID_FERMIER;
                     		produit_M.NB_VENTE := produit.NB_VENTE;
                     		produit_M.PRIX := produit.PRIX;
                  		END IF;
                  		exit when produits%NOTFOUND;
               		END LOOP;
               		CLOSE produits;
               		-- Si ce fermier seul satisfait la demande
               		IF (produit_M.NB_VENTE >= quant)
					THEN
                  		IF(produit_M.NB_VENTE = quant)
						THEN --offre==demande => supp du vendeur de cette categorie
                     		DELETE FROM MARCHE_VEND WHERE id_art = MARCHE_VEND.ID_ARTICLE and produit_M.ID_FERMIER = MARCHE_VEND.ID_FERMIER;
                  		ELSE --offre > demande => mis a jour
                     		UPDATE MARCHE_VEND set MARCHE_VEND.NB_VENTE = MARCHE_VEND.NB_VENTE-quant WHERE id_art=MARCHE_VEND.ID_ARTICLE and produit_M.ID_FERMIER = MARCHE_VEND.ID_FERMIER;
                  		END IF;
						-- MaJ Compte Bancaire
                  		UPDATE FERME set FERME.ECUS=FERME.ECUS+(quant*prix) WHERE FERME.ID_FERMIER=produit_M.ID_FERMIER;
                  		UPDATE FERME set FERME.ECUS=FERME.ECUS-(quant*prix) WHERE FERME.ID_FERMIER=id_fer;
						-- VerIFier si on a deja d'autres articles du mm type
                  		SELECT count(ID_ARTICLE) INTO tmp FROM ENTREPOSE WHERE id_fer=ENTREPOSE.ID_FERMIER and id_art=ENTREPOSE.ID_ARTICLE;
                  		IF(tmp = 0)
						THEN
                     		INSERT INTO ENTREPOSE values(id_fer, id_art, quant, 'F');
                  		ELSE
                     		UPDATE ENTREPOSE set ENTREPOSE.QUANTITE=ENTREPOSE.QUANTITE+quant WHERE id_art=ENTREPOSE.ID_ARTICLE and produit_M.ID_FERMIER = ENTREPOSE.ID_FERMIER;
                  		END IF;
						-- Idem pr Vendeur
                  		SELECT count(ID_ARTICLE) INTO tmp FROM A_VENDU WHERE id_fer=A_VENDU.ID_FERMIER and id_art=A_VENDU.ID_ARTICLE;
                  		IF(tmp = 0) 
						THEN
                     		INSERT INTO A_VENDU values(id_fer, id_art, quant, CURRENT_DATE);
                  		ELSE
                     		UPDATE A_VENDU set A_VENDU.NB_VENDU=A_VENDU.NB_VENDU+quant, DATE_VENTE=CURRENT_DATE WHERE id_art=A_VENDU.ID_ARTICLE and produit_M.ID_FERMIER = A_VENDU.ID_FERMIER;
                  		END IF;
						fait := quant;
               		ELSE --ce fermier seul ne satisfait pas la demande
                  		DELETE FROM MARCHE_VEND WHERE id_art=MARCHE_VEND.ID_ARTICLE and produit_M.ID_FERMIER = MARCHE_VEND.ID_FERMIER;
                  		UPDATE FERME set FERME.ECUS=FERME.ECUS+(produit_M.NB_VENTE*prix) WHERE FERME.ID_FERMIER=produit_M.ID_FERMIER;
                  		UPDATE FERME set FERME.ECUS=FERME.ECUS-(produit_M.NB_VENTE*prix) WHERE FERME.ID_FERMIER=id_fer;
                  		SELECT count(ID_ARTICLE) INTO tmp FROM ENTREPOSE WHERE id_fer=ENTREPOSE.ID_FERMIER and id_art=ENTREPOSE.ID_ARTICLE;
                  		IF(tmp = 0) 
						THEN
                     		INSERT INTO ENTREPOSE values(id_fer, id_art, produit_M.NB_VENTE, 'F');
                  		ELSE
                     		UPDATE ENTREPOSE set ENTREPOSE.QUANTITE=ENTREPOSE.QUANTITE+produit_M.NB_VENTE WHERE id_art=ENTREPOSE.ID_ARTICLE and produit_M.ID_FERMIER = ENTREPOSE.ID_FERMIER;
                  		END IF;
                  		SELECT count(ID_ARTICLE) INTO tmp FROM A_VENDU WHERE id_fer=A_VENDU.ID_FERMIER and id_art=A_VENDU.ID_ARTICLE;
                  		IF(tmp = 0) 
						THEN
                     		INSERT INTO A_VENDU values(id_fer, id_art, produit_M.NB_VENTE, CURRENT_DATE);
                  		ELSE
                     		UPDATE A_VENDU set A_VENDU.NB_VENDU=A_VENDU.NB_VENDU+produit_M.NB_VENTE, DATE_VENTE=CURRENT_DATE WHERE id_art=A_VENDU.ID_ARTICLE and produit_M.ID_FERMIER = A_VENDU.ID_FERMIER;
                  		END IF;
                  		fait := produit_M.NB_VENTE;
               		END IF;
            		END loop;
	         	ELSE
				RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
			END IF;
		ELSE
			RAISE_APPLICATION_ERROR(-20114,'Vous n`avez pas les droits necessaires');
	    END IF;
   	ELSE
	    RAISE_APPLICATION_ERROR(-20113,'Trop d`articles vendus');
   	END IF;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE Acheter_articleC																-- Checked
(
	idfermier FERME.ID_FERMIER%type,
	idarticle article.id_article%type,
	quantite INTEGER
) AS
	ferm   FERME%rowtype ;
	stock  ENTREPOSE%rowtype ;
	coop   ARTICLE%rowtype ;
	vente  A_VENDU%rowtype ;
	achat  MARCHE_VEND%rowtype;
	Cursor curVente IS 
		SELECT * FROM a_vendu WHERE a_vendu.id_fermier = idfermier;
	Cursor curAchat IS 
		SELECT * FROM marche_vend WHERE marche_vend.id_fermier = idfermier;
	nbMaxAchat   FERME.NB_ACHATS_JOUR%type;
	solde        FERME.ECUS%type;
	quant        ARTICLE.QUANTITE%type;
	fermQuant    ARTICLE.QUANTITE%type;
	prix         ARTICLE.PRIX_VENTE%type;
	prixTotal integer;
	noHeure   number;
	dateActu  date;
	jour      varchar(20) ;
	heure     varchar(20);
	ouverte   number;
	nbPoules  number ;
	nbLapins  number;
	nbVendus  number;
	nbLitres  number ;
	nbAchats  number ;
	animaux   number ;
	ageV      number;
	nbLF      number;
	nbLM      number;
BEGIN
	SELECT * INTO ferm FROM FERME WHERE idfermier = FERME.ID_FERMIER;
	SELECT * INTO coop FROM article WHERE ARTICLE.id_article = idarticle;
	SELECT * INTO stock FROM entrepose WHERE ENTREPOSE.id_fermier = idfermier and id_article= idarticle;

	nbMaxAchat := ferm.NB_ACHATS_JOUR;
	solde := ferm.ECUS;
	quant := coop.QUANTITE;
	prix := coop.PRIX_VENTE;
	prixTotal :=  prix*quantite;
	-- Nb Lapins
	nbLapins := ferm.NB_MALE + ferm.NB_FEMELLE ;
	-- Compter le nb d'achats
	nbAchats := 0;
	FOR achat IN curAchat
	LOOP
		nbAchats := nbAchats + achat.nb_vente ;
	END LOOP;
	-- Variables pour gerer les ouvertures et fermetures de la coop
	SELECT current_date INTO dateActu FROM dual;
	heure := TO_CHAR(dateActu,'HH24');
	jour := TO_CHAR(dateActu,'DY');
	noHeure := TO_NUMBER(heure);

	-- Coop ouverte ?
	IF((jour = 'SAM.') or (jour = 'DIM.'))
	THEN
		IF(((noHeure >= 9) and (noHeure < 14)) or (noHeure > 19) or (noHeure < 3))
		THEN
			ouverte := 1;
		ELSE
			ouverte := 0;
          	END IF ;
	ELSE
		IF (((noHeure >= 5) and (noHeure < 14)) or ((noHeure >= 17) and (noHeure<20)) or(noHeure >= 22)or(noHeure < 3))
		THEN
			ouverte := 1;
		ELSE
			ouverte := 0;
		END IF;
	END IF;

	-- Si coop ouverte
	IF(ouverte = 1)
	THEN
		-- Achats encore possibles
		IF (nbMaxAchat > quantite)
		THEN
			-- Article en stock
			IF (quantite < quant)
			THEN
				-- Assez d'argent
				IF(solde > prixTotal)
				THEN
					-- mise a jour des donnees
					solde := solde - prixTotal ;
					UPDATE FERME set FERME.ECUS = solde WHERE id_fermier = idfermier;
					quant := quant - quantite ;
					UPDATE ARTICLE set ARTICLE.QUANTITE = quant WHERE id_article = idarticle;
					nbMaxAchat := nbMaxAchat - quantite;
					UPDATE FERME set FERME.NB_ACHATS_JOUR = nbMaxAchat WHERE FERME.ID_FERMIER = idfermier;
					SELECT count(ID_ARTICLE) INTO fermQuant FROM ENTREPOSE WHERE ENTREPOSE.ID_FERMIER = idfermier and ENTREPOSE.ID_ARTICLE= idarticle;
					-- Nb ds entrepot
                 	IF(fermQuant = 0)
					THEN
                    	INSERT INTO ENTREPOSE values(idfermier, idarticle, quantite, 'F');
						-- gestion des poules et des coqs
						IF((idarticle = jmeumeu.id_coq) or (idarticle = jmeumeu.id_poule))
						THEN
							SELECT  count(AGE_V) INTO ageV FROM VOLAILLE WHERE VOLAILLE.ID_FERMIER =idfermier and VOLAILLE.ID_VOLAILLE =idarticle;
							IF(ageV = 0)
							THEN
								INSERT INTO VOLAILLE values(idarticle,idfermier,'F',0,'F',0,'F',0,2.5,4,0);
							END IF;
						ELSE
						-- gestion des lapins et lapines
							IF((idarticle = jmeumeu.id_lapin_male) or (idarticle = jmeumeu.id_lapin_femelle))
							THEN
								IF(idarticle = jmeumeu.id_lapin_male)
								THEN
									UPDATE FERME set FERME.NB_MALE = quantite WHERE FERME.ID_FERMIER = idfermier;
								ELSE
									UPDATE FERME set FERME.NB_FEMELLE = quantite WHERE FERME.ID_FERMIER = idfermier;
								END IF;
							END IF;
						END IF;
                  	ELSE
						-- gestion generale des poules et coqs
						IF((idarticle = jmeumeu.id_coq) or (idarticle = jmeumeu.id_poule) or (idarticle = jmeumeu.id_poussin_male) or (idarticle = jmeumeu.id_poussin_femelle))
						THEN
							IF((fermQuant + quantite) > 60)
							THEN
								RAISE_APPLICATION_ERROR(-20227,'Capacite maximale atteinte');
							END IF;
						ELSE
							-- gestion generale des lapins et lapines
							IF(((idarticle =jmeumeu.id_lapin_male) or (idarticle = jmeumeu.id_lapin_femelle)) and ((nbLapins+quantite)<40))
							THEN
								IF(idarticle = jmeumeu.id_lapin_male)
								THEN
									SELECT NB_MALE INTO nbLM FROM FERME WHERE FERME.ID_FERMIER =idfermier ;
									nbLM := nbLM +quantite ;
									UPDATE FERME set FERME.NB_MALE = nbLM WHERE FERME.ID_FERMIER = idfermier;
								ELSE
									SELECT NB_FEMELLE INTO nbLF FROM FERME WHERE FERME.ID_FERMIER =idfermier ;
									nbLF := nbLF +quantite ;
									UPDATE FERME set FERME.NB_FEMELLE = nbLF WHERE FERME.ID_FERMIER = idfermier;
								END IF;
							ELSE
								RAISE_APPLICATION_ERROR(-20227,'Capacite maximale atteinte');
							END IF;
						END IF;
						-- Gestion stock
						fermQuant := stock.QUANTITE;
						fermQuant := fermQuant + quantite ;
                     	UPDATE ENTREPOSE set ENTREPOSE.QUANTITE = fermQuant WHERE id_fermier = idfermier and id_article= idarticle;
					END IF;
				ELSE
					RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
				END IF;
			ELSE
				RAISE_APPLICATION_ERROR(-20113,'Trop d`articles vendus');
			END IF;
		ELSE
			RAISE_APPLICATION_ERROR(-20115,'Depassement de la limite de ventes/achats par jour');
 		END IF;
	ELSE
		RAISE_APPLICATION_ERROR(-20116,'Coop ferme');
	END IF;
	COMMIT;
END;
/






























-- Groupe 3				CHECKED
CREATE OR REPLACE PROCEDURE nourrir_clapier																	-- Checked
(
	idfermier ferme.id_fermier%type,
	young boolean
) AS
	nb_ecus ferme.ecus%type;
	nourrij ferme.nourri_j%type;
	nourril ferme.nourri_l%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 5;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		if young then
			SELECT nourri_j into nourrij FROM ferme WHERE idfermier = id_fermier;
			if nourrij = 'T' then
				RAISE_APPLICATION_ERROR(-20222,'Animaux deja nourris');
			else
				UPDATE ferme set nourri_j = 'T' WHERE idfermier = id_fermier;
			end if;
		else
			SELECT nourri_l into nourril from ferme WHERE idfermier = id_fermier;
			if nourril = 'T' then
				RAISE_APPLICATION_ERROR(-20222,'Animaux deja nourris');
			else
				UPDATE ferme set nourri_l = 'T' WHERE idfermier = id_fermier;
			end if;
		end if;
		UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE abreuver_clapier																-- Checked
(
	idfermier ferme.id_fermier%type,
	young boolean
) AS
	nb_ecus ferme.ecus%type;
	abreuvagej ferme.abreuvage_j%type;
	abreuvagel ferme.abreuvage_l%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 2;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		if young then
			SELECT abreuvage_j into abreuvagej FROM ferme WHERE idfermier = id_fermier;
			if abreuvagej = 'T' then
				RAISE_APPLICATION_ERROR(-20223,'Animaux deja abreuves');
			else
				UPDATE ferme set abreuvage_j = 'T' WHERE idfermier = id_fermier;
			end if;
		else
			SELECT abreuvage_l into abreuvagel FROM ferme WHERE idfermier = id_fermier;
			if abreuvagel = 'T' then
				RAISE_APPLICATION_ERROR(-20223,'Animaux deja abreuves');
			else
				UPDATE ferme set abreuvage_l = 'T' WHERE idfermier = id_fermier;
			end if;
		end if;
		UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE nettoyer_clapier																-- Checked
(
	idfermier ferme.id_fermier%type,
	young boolean
) AS
	nb_ecus ferme.ecus%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 3;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		if young then
			UPDATE ferme set sale_j = 'F' WHERE idfermier = id_fermier;
		else
			UPDATE ferme set sale_l = 'F' WHERE idfermier = id_fermier;
		end if;
		UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE soigner_clapier																	-- Checked
(
	idfermier ferme.id_fermier%type,
	young boolean
) AS
	nb_ecus ferme.ecus%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 6;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		if young then
			UPDATE ferme set nb_malade_j = 0 WHERE idfermier = id_fermier;
		else
			UPDATE ferme set nb_malade_l = 0 WHERE idfermier = id_fermier;
		end if;
		UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE vendre_lapins																	-- Checked
(
	idfermier ferme.id_fermier%type,
	male boolean,
	nb integer
) AS
	nb_lapins integer;
BEGIN
	if male then
		SELECT nb_male into nb_lapins FROM ferme WHERE idfermier=id_fermier;
		nb_lapins := nb_lapins - nb;
		if nb_lapins < 0 then
			RAISE_APPLICATION_ERROR(-20224,'Pas assez d`animaux');
		end if;
		UPDATE ferme set nb_male = nb_lapins WHERE idfermier=id_fermier;
		INSERT INTO a_vendu VALUES (idfermier,jmeumeu.id_lapin_male,nb,CURRENT_DATE);
	else
		SELECT nb_femelle into nb_lapins FROM ferme WHERE idfermier=id_fermier;
		nb_lapins := nb_lapins - nb;
		if nb_lapins < 0 then
			RAISE_APPLICATION_ERROR(-20224,'Pas assez d`animaux');
		end if;
		UPDATE ferme set nb_femelle = nb_lapins WHERE idfermier=id_fermier;
		INSERT INTO a_vendu VALUES (idfermier,jmeumeu.id_lapin_femelle,nb,CURRENT_DATE);
	end if;
	UPDATE ferme set ecus=ecus+25*nb WHERE idfermier=id_fermier;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE naissance_lapins AS																-- Checked
	nb_naissances integer;
	surplus integer;
BEGIN
	for r in (SELECT id_fermier, nb_male, nb_femelle, nb_moyen, nb_gros FROM ferme WHERE hibernation = 'F' and nb_male > 0 and nb_femelle > 0 and nb_moyen + nb_gros < 50) loop
		if r.nb_male < r.nb_femelle then
			nb_naissances := r.nb_male * (7/9);
		else
			nb_naissances := r.nb_femelle * (7/9);
		end if;
		surplus := nb_naissances + r.nb_moyen + r.nb_gros - 50;
		if surplus > 0 then
			nb_naissances := nb_naissances - surplus;
		end if;
		UPDATE ferme set nb_petit=nb_naissances WHERE id_fermier = r.id_fermier;
	end loop;
	COMMIT;
END;
/





CREATE OR REPLACE FUNCTION aff_clapier																		-- Checked
(
	idfermier ferme.id_fermier%type
)
	RETURN sys_refcursor
AS
	curseur sys_refcursor;
BEGIN
	OPEN curseur FOR
		SELECT nb_male,nb_femelle,nb_petit,nb_moyen,nb_gros,abreuvage_l,nb_abreuvage_l,nourri_l,nb_jeune_l,sale_l,nb_malade_l,abreuvage_j,nb_abreuvage_j,nourri_j,nb_jeune_j,sale_j,nb_malade_j
		FROM ferme WHERE id_fermier=idfermier;
	return curseur;
END;
/





CREATE OR REPLACE FUNCTION aff_remise																		-- Checked
(
	idfermier entrepose.id_fermier%type
)
	RETURN sys_refcursor
AS
	curseur sys_refcursor;
BEGIN
	OPEN curseur FOR
		SELECT article.id_article,nom,description,collectionnable,article.quantite,prix_vente,prix_achat,entrepose.quantite
		FROM entrepose,article WHERE id_fermier=idfermier and collection = 'F' and entrepose.id_article=article.id_article;
	return curseur;
END;
/

























-- Groupe 4				CHECKED
CREATE OR REPLACE PROCEDURE Creation_Compte																	-- Checked
(
	login FERME.ID_FERMIER%type,
	nom FERME.NOM%type,
	pwd FERME.MDP%type,
	email FERME.MAIL%type
) AS
BEGIN
	INSERT INTO FERME VALUES (login,nom,pwd,email,CURRENT_DATE,CURRENT_DATE,1500,0,NULL,'F',0,0,0,0,0,8,'F',0,'F',0,'F',0,'F', 0,'F',0,'F',0,'F',0,'F',0,'F',0,0,0,0);
	COMMIT;
EXCEPTION
	WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20001,'Login deja utilisee');
END;
/





CREATE OR REPLACE PROCEDURE Suppression_Compte																-- Checked
(
	login FERME.ID_FERMIER%type
) AS
BEGIN 
	DELETE FROM FERME WHERE login = Ferme.id_fermier ;
	COMMIT;
EXCEPTION
	WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20002,'Compte Inexistant');
END;
/





CREATE OR REPLACE PROCEDURE Hiberner AS																		-- Checked
	x number := 0 ;
BEGIN
	for uneFerme in(SELECT date_connection,id_fermier FROM ferme) loop
		x := trunc(CURRENT_DATE) - trunc(UneFerme.date_connection) ;
		if (x > 3) then
			UPDATE Ferme set Hibernation = 'T' WHERE id_fermier = uneFerme.id_fermier;
		elsif x > 50 then
			Suppression_Compte(uneFerme.id_fermier);
		end if ;
	end loop;
	COMMIT;
END;
/





CREATE OR REPLACE FUNCTION Aff_poulailler																	-- Checked
(
	idfermier ferme.id_fermier%type
)
	RETURN SYS_REFCURSOR
AS
	curseur SYS_REFCURSOR;
BEGIN
	OPEN curseur FOR
		SELECT id_volaille,abreuvage_v,nourri_v,sale_v,nb_malade_v,production_v
		FROM Volaille WHERE id_fermier=idfermier;
	RETURN curseur;
END;
/





CREATE OR REPLACE FUNCTION Aff_Collection																	-- Checked
(
	idfermier Ferme.id_fermier%type
)
    RETURN SYS_REFCURSOR
AS
    curseur SYS_REFCURSOR;
BEGIN
    OPEN curseur FOR
        SELECT e.ID_ARTICLE, NOM, DESCRIPTION, e.QUANTITE
		FROM ENTREPOSE e, ARTICLE a WHERE idfermier = e.id_fermier and e.id_article = a.id_article and a.collectionnable = 'T' ;
    RETURN curseur;
END;
/





CREATE OR REPLACE PROCEDURE Connection																		-- Checked
(
	login Ferme.id_fermier%type,
	motdepasse Ferme.mdp%type
) AS
BEGIN
	UPDATE Ferme set date_connection = CURRENT_DATE, Hibernation = 'F' WHERE login = id_fermier and motdepasse = mdp;
	COMMIT;
EXCEPTION
	WHEN OTHERS THEN RAISE_APPLICATION_ERROR(-20003,'Connexion Impossible');
END;
/



























-- Groupe 5				CHECKED
CREATE OR REPLACE PROCEDURE vendre_oeufs																	-- Checked
(
	volaile volaille.id_volaille%type,
	fermier ferme.id_fermier%type
) AS
	nb_oeufs volaille.production_v%type;
	nb_ecus ferme.ecus%type;
BEGIN
	SELECT PRODUCTION_V INTO nb_oeufs FROM Volaille WHERE id_fermier=fermier and id_volaille=volaile;
	IF nb_oeufs > 0 THEN
		SELECT ecus INTO nb_ecus FROM Ferme WHERE id_fermier=fermier;
		UPDATE Ferme SET ECUS=nb_ecus+nb_oeufs*8 WHERE id_fermier=fermier;
		UPDATE Volaille SET PRODUCTION_V=0 WHERE id_fermier=fermier and id_volaille=volaile;
	ELSE
		RAISE_APPLICATION_ERROR(-20333,'Aucun oeuf');
	END IF;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE abreuver_volaille																-- Checked
(
	idvolaille volaille.id_volaille%type,
	idfermier ferme.id_fermier%type
) AS
	nb_ecus ferme.ecus%type;
	abreuvage volaille.abreuvage_v%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 1;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		SELECT abreuvage_v into abreuvage FROM volaille WHERE idfermier = id_fermier and idvolaille = id_volaille;
		if abreuvage = 'T' then
			RAISE_APPLICATION_ERROR(-20223,'Animaux deja abreuves');
		else
			UPDATE volaille set abreuvage_v = 'T' WHERE idfermier = id_fermier and idvolaille = id_volaille;
			UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
		end if;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE nourrir_volaille																-- Checked
(
	idvolaille volaille.id_volaille%type,
	idfermier ferme.id_fermier%type
) AS
	nb_ecus ferme.ecus%type;
	nourri volaille.nourri_v%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = idfermier;
	nb_ecus := nb_ecus - 3;
	if nb_ecus < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		SELECT nourri_v into nourri FROM volaille WHERE idfermier = id_fermier and idvolaille = id_volaille;
		if nourri = 'T' then
			RAISE_APPLICATION_ERROR(-20222,'Animaux deja nourris');
		else
			UPDATE volaille set nourri_v = 'T' WHERE idfermier = id_fermier and idvolaille = id_volaille;
			UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
		end if;
	end if;
	COMMIT;
END;
/





CREATE or REPLACE PROCEDURE nettoyage_volaille																-- Checked
(
	idfermier ferme.id_fermier%type,
	idvolaille volaille.id_volaille%type
) AS
	nb_ecus ferme.ecus%type;
	sale volaille.SALE_V%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE id_fermier = idfermier;
	SELECT sale_v into sale FROM volaille WHERE id_fermier = idfermier and id_volaille = idvolaille;
	IF sale = 'T' THEN
		IF nb_ecus < 3 THEN
			RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
		ELSE
			nb_ecus := nb_ecus - 3;
			UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
			UPDATE volaille set sale_v = 'F' WHERE id_fermier = idfermier and id_volaille = idvolaille;
		END IF;
	END IF;
	COMMIT;
END;
/





CREATE or REPLACE PROCEDURE soin_volaille																	-- Checked
(
	idfermier ferme.id_fermier%type,
	idvolaille volaille.id_volaille%type
) AS
	nb_ecus ferme.ecus%type;
	nb_malade volaille.NB_MALADE_V%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE id_fermier = idfermier;
	SELECT nb_malade_v into nb_malade FROM volaille WHERE id_fermier = idfermier and id_volaille = idvolaille;
	IF (nb_malade > 0 and nb_malade < 4) THEN
		IF nb_ecus < 6 THEN
			RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
		ELSE
	        nb_ecus := nb_ecus - 6;
	        UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = idfermier;
	        UPDATE volaille set nb_malade_v = 0 WHERE id_fermier = idfermier and id_volaille = idvolaille;
		END IF;
	END IF;
	COMMIT;
END;
/





CREATE or REPLACE PROCEDURE creation_oeuf																	-- Checked
(
	volaile volaille.id_volaille%type,
	fermier ferme.id_fermier%type
) AS
	poids volaille.poids_v%type;
	age volaille.age_v%type;
	sale volaille.sale_v%type;
	malade volaille.nb_malade_v%type;
	rnd NUMBER;
BEGIN
	SELECT POIDS_V INTO poids FROM Volaille WHERE id_fermier=fermier and id_volaille=volaile;
	SELECT AGE_V INTO age FROM Volaille WHERE id_fermier=fermier and id_volaille=volaile;
	SELECT SALE_V INTO sale FROM Volaille WHERE id_fermier=fermier and id_volaille=volaile;
	SELECT NB_MALADE_V INTO malade FROM Volaille WHERE id_fermier=fermier and id_volaille=volaile;

	IF poids < 2.5 THEN
		RAISE_APPLICATION_ERROR(-20334,'Trop maigre');
	END IF;
	IF age < 5 THEN
		RAISE_APPLICATION_ERROR(-20335,'Trop Jeune');
	END IF;
	IF sale = 'T' THEN
		RAISE_APPLICATION_ERROR(-20225,'Animal Sale');
	END IF;
	IF malade > 0 THEN
		RAISE_APPLICATION_ERROR(-20226,'Animal Malade');
	END IF;

	SELECT dbms_random.value into rnd FROM dual;
	-- Mise à jour des tables
	IF rnd < 0.1 THEN
		-- ne ponds pas
		UPDATE Volaille SET PRODUCTION_V=PRODUCTION_V WHERE id_fermier=fermier and id_volaille=volaile;
	ELSE
		IF rnd < 0.5 THEN
			-- pond 2 oeufs
			UPDATE Volaille SET PRODUCTION_V=PRODUCTION_V+2 WHERE id_fermier=fermier and id_volaille=volaile;
		ELSE
			-- pond 1 oeuf
			UPDATE Volaille SET PRODUCTION_V=PRODUCTION_V+1 WHERE id_fermier=fermier and id_volaille=volaile;
		END IF;
	END IF;
	COMMIT;
END;
/























-- Groupe 6				CHECKED
CREATE OR REPLACE PROCEDURE NettoyerVache																	-- Checked
(
	IdFermier ferme.id_fermier%type
) AS
	Money ferme.ecus%type;
BEGIN
    SELECT ecus into Money FROM ferme WHERE ferme.id_fermier = IdFermier;
	Money := Money - 3;
	if (Money < 0) then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		UPDATE ferme set sale_vache = 'F' WHERE IdFermier = id_fermier;
		UPDATE ferme set ecus = Money WHERE ferme.id_fermier = IdFermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE SoignerVache																	-- Checked
(
	IdFermier ferme.id_fermier%type
) AS
    Money ferme.ecus%type;
BEGIN
	SELECT ecus into Money FROM ferme WHERE ferme.id_fermier = IdFermier;
	Money := Money - 6;
	if (Money < 0) then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		UPDATE ferme set nb_malade_vache = '0' WHERE IdFermier = id_fermier;
		UPDATE ferme set ecus = Money WHERE ferme.id_fermier = IdFermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE nourrir_vache																	-- Checked
(
	idfermier ferme.id_fermier%type
) AS
	Cout ferme.ecus%type;
	nourriv ferme.nourri_vache%type;
BEGIN
	SELECT ecus into Cout FROM ferme WHERE ferme.id_fermier = idfermier;
	Cout := Cout - 5;
	if Cout < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		SELECT nourri_vache into nourriv FROM ferme WHERE idfermier = id_fermier;
		if nourriv = 'T' then
			RAISE_APPLICATION_ERROR(-20222,'Animaux deja nourris');
		else
			UPDATE ferme set nourri_vache = 'T' WHERE idfermier = id_fermier;
			UPDATE ferme set ecus = Cout WHERE ferme.id_fermier = idfermier;
		end if;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE abreuver_vache																	-- Checked
(
	idfermier ferme.id_fermier%type
) AS
	Cout ferme.ecus%type;
	abreuvagev ferme.abreuvage_vache%type;
BEGIN
	SELECT ecus into Cout FROM ferme WHERE ferme.id_fermier = idfermier;
	Cout := Cout - 2;
	if Cout < 0 then
		RAISE_APPLICATION_ERROR(-20111,'Pas assez d`argent');
	else
		SELECT abreuvage_vache into abreuvagev FROM ferme WHERE idfermier = id_fermier;
		if abreuvagev = 'T' then
			RAISE_APPLICATION_ERROR(-20223,'Animaux deja abreuves');
		else
			UPDATE ferme set abreuvage_vache = 'T' WHERE idfermier = id_fermier;
			UPDATE ferme set ecus = Cout WHERE ferme.id_fermier = idfermier;
		end if;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE creation_lait																	-- Checked
(
	idfermier ferme.id_fermier%type
) AS
	lait ferme.NB_LAIT%type;
	sale_vache ferme.SALE_VACHE%type;
	malade_vache ferme.NB_MALADE_VACHE%type;
	poids ferme.POIDS_VACHE%type;
	age ferme.AGE_VACHE%type;
BEGIN
	SELECT SALE_VACHE into sale_vache FROM ferme WHERE ferme.id_fermier = IdFermier;
	SELECT NB_MALADE_VACHE into malade_vache FROM ferme WHERE ferme.id_fermier = IdFermier;
	SELECT NB_LAIT into lait FROM ferme WHERE ferme.id_fermier = IdFermier;
	SELECT POIDS_VACHE into poids FROM ferme WHERE ferme.id_fermier = IdFermier;
	SELECT AGE_VACHE into age FROM ferme WHERE ferme.id_fermier = IdFermier;

	-- Vache tjs nourrie
	if (malade_vache = 0 and sale_vache = 'F') then
		if ((poids >= 80) and (age >= 10)) then
			if (lait = 0) then 
				lait := lait + 8 ;
			else
				lait := lait + 4 ;
			end if;
			if (lait > 16) then
				lait := 16;
			end if;
			UPDATE ferme set NB_LAIT = lait WHERE ferme.id_fermier = IdFermier;
		end if;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE PROCEDURE traire_lait																		-- Checked
(
	idfermier ferme.id_fermier%type
) AS
	nb_ecus ferme.ecus%type;
	nblait ferme.NB_LAIT%type;
BEGIN
	SELECT ecus into nb_ecus FROM ferme WHERE ferme.id_fermier = IdFermier;
	SELECT NB_LAIT into nblait FROM ferme WHERE ferme.id_fermier = IdFermier;

	-- Traire et vendre le lait
	if (nblait <= 0) then
		RAISE_APPLICATION_ERROR(-20444,'Pas de Lait');
	else
		nb_ecus := nb_ecus + (2 * nblait);
		UPDATE ferme set ecus = nb_ecus WHERE ferme.id_fermier = IdFermier;
		UPDATE ferme set NB_LAIT = 0 WHERE ferme.id_fermier = IdFermier;
	end if;
	COMMIT;
END;
/





CREATE OR REPLACE FUNCTION aff_paturage																		-- Checked
(
	IdFermier ferme.id_fermier%type
)
	RETURN sys_refcursor
AS
	curseur sys_refcursor;
BEGIN
	OPEN curseur FOR
		SELECT ABREUVAGE_VACHE, NB_ABREUVAGE_VACHE, NOURRI_VACHE, NB_JEUNE_VACHE,SALE_VACHE, NB_MALADE_VACHE, POIDS_VACHE, AGE_VACHE, NB_LAIT
		FROM ferme WHERE id_fermier = IdFermier;
	RETURN curseur;
END;
/







