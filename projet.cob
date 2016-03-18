IDENTIFICATION DIVISION.
PROGRAM-ID. projet.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT fscenes ASSIGN TO "scenes.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS fs_stat
       RECORD KEY IS fs_cleSce
       ALTERNATE RECORD KEY IS fs_dateA WITH DUPLICATES.


       SELECT freservations ASSIGN TO "reservations.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS fres_stat
       RECORD KEY IS fres_id
       ALTERNATE RECORD KEY IS fres_dateA WITH DUPLICATES
       ALTERNATE RECORD KEY IS fres_nomPa WITH DUPLICATES
       ALTERNATE RECORD KEY IS fres_dep WITH DUPLICATES.

       SELECT fpass ASSIGN TO "pass.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS fp_stat
       RECORD KEY IS fp_clePass
       ALTERNATE RECORD KEY IS fp_dateA WITH DUPLICATES.
       
       SELECT fgroupes ASSIGN TO "groupes.dat"
       ORGANIZATION SEQUENTIAL
       ACCESS IS SEQUENTIAL
       FILE STATUS IS fg_stat.

       SELECT fgroupesTemp ASSIGN TO "groupes_temp.dat"
       ORGANIZATION SEQUENTIAL
       ACCESS IS SEQUENTIAL
       FILE STATUS IS fgTemp_stat.

       SELECT frepresentations ASSIGN TO "representations.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS frep_stat
       RECORD KEY IS frep_cleRep
       ALTERNATE RECORD KEY IS frep_dateA WITH DUPLICATES
       ALTERNATE RECORD KEY IS frep_nomGr WITH DUPLICATES
       ALTERNATE RECORD KEY IS frep_nomSce WITH DUPLICATES.


       SELECT feditions ASSIGN TO "editions.dat"
       ORGANIZATION INDEXED
       ACCESS IS DYNAMIC
       FILE STATUS IS fe_stat
       RECORD KEY IS fe_dateA.

       SELECT fincrements ASSIGN TO "increment.dat"
       ORGANIZATION SEQUENTIAL 
       ACCESS IS SEQUENTIAL 
       FILE STATUS IS fi_stat.
       

DATA DIVISION.
FILE SECTION.
       

        FD fincrements. 
        01 finTampon.
          02 fi_idResa PIC 9(36).

        FD fscenes.
        01 fscTampon.
          02 fs_cleSce. 
            03 fs_nomSce PIC A(30).
            03 fs_dateA PIC 9(4).
          02 fs_capacite PIC 9(2).
          02 fs_cout  PIC 9(6).  


        FD freservations.
        01 fresTampon. 
          02 fres_id            PIC 9(36).
          02 fres_nomPa         PIC A(30).
          02 fres_prenom        PIC A(30).
          02 fres_dep           PIC X(2).
          02 fres_dateA         PIC 9(4).
          02 fres_adresseEmail  PIC X(30).
          02 fres_numTel        PIC XXXXXXXXXX.
          02 fres_dateNaissance PIC 9(8). 

        FD fpass.
        01 passTampon.
          02 fp_clePass. 
            03 fp_nomPa PIC A(30).
            03 fp_dateA PIC 9(4).
          02 fp_prix PIC 9(4).

        FD fgroupes.
        01 fgTampon.
          02 fg_nom PIC A(30).
          02 fg_style PIC A(30).

       FD fgroupesTemp.
        01 fgTamponTemp.
          02 fg_nomT PIC A(30).
          02 fg_styleT PIC A(30).

        FD frepresentations.
        01 frepTampon.
          02 frep_cleRep. 
            03 frep_jour  PIC 9(2).
            03 frep_heureDebut PIC 9(4).
            03 frep_dateA PIC 9(4).
            03 frep_nomSce PIC A(30).
          02 frep_nomGr PIC A(30).
          02 frep_cachet PIC 9(6).
          02 frep_nbPersonneMax PIC 9(30).


         FD feditions. 
         01 fedTampon. 
          02 fe_dateA PIC 9(4). 
          02 fe_capacite PIC 9(6).
          02 fe_nbJour PIC 9(2). 
          02 fe_NbScene PIC 9(2).
          02 fe_nbArtiste PIC 9(3).
          02 fe_nbResaJourUn PIC 9(4).
          02 fe_nbResaJourDeux PIC 9(4). 
          02 fe_nbResaJourTrois PIC 9(4). 
          02 fe_resultat PIC 9(30). 
          02 fe_coutMoyenScene PIC 9(30). 
          02 fe_coutArtistes PIC 9(30). 
                  
WORKING-STORAGE SECTION.
      *> Déclarations des zones de compte rendu  
        77 fs_stat PIC 9(2). 
        77 fp_stat PIC 9(2).
        77 fres_stat PIC 9(2).
        77 fg_stat PIC 9(2).
        77 fgTemp_stat PIC 9(2).
        77 frep_stat PIC 9(2).
        77 fe_stat PIC 9(2).
        77 fi_stat PIC 9(2). 

      *> Variables globales   
        77 choixMenu PIC 9(2).
        77 choix     PIC 9(2).
        77 Wfin      PIC 9.

    	*>Variables pass réservation
        77 nomPa     PIC A(30).
        77 dateA     PIC 9(4).
        77 dep       PIC 9(2).
        77 j         PIC 99.
        77 m         PIC 99.
        77 y         PIC 9999.
        77 Wtrouve   PIC 9(1).
    	*>Variables groupe représentation
        77 nomGr PIC A(30).
        77 pos PIC 9.
        77 posFin PIC 9.
        77 nomDernier PIC A(30).
        77 styleDernier PIC A(30).
        77 choixModifReserv PIC 9(2).
    	*>Variable scenes
      *> SUPPRIMER_SCENE
        77 WrepSc PIC 9(1).
        *>AJOUT_SCENES 
        77 WnomSc PIC A(4).
     *> MODIFIER_SCENE
        77 Wprog PIC 9(1).
     *> ANNEE 
        77 Wyear PIC 9(4).
        77 conf PIC 9(1). 
        77 Wrep PIC 9(1).
PROCEDURE DIVISION.

      *> Vérifiaction présence des fichiers 
       OPEN I-O fscenes
       IF fs_stat =35 THEN
              OPEN OUTPUT fscenes
              CLOSE fscenes    
       ELSE 
              CLOSE fscenes
       END-IF

       OPEN I-O fpass
       IF fp_stat =35 THEN
              OPEN OUTPUT fpass
              CLOSE fpass    
       ELSE 
              CLOSE fpass
       END-IF

       OPEN EXTEND fgroupes
       IF fg_stat = 35 THEN
         OPEN OUTPUT fgroupes
         CLOSE fgroupes
       ELSE 
         CLOSE fgroupes
       END-IF

       OPEN EXTEND fincrements
       IF fi_stat = 35 THEN
         OPEN OUTPUT fincrements
          MOVE 0 TO fi_idResa
          WRITE finTampon END-WRITE
         CLOSE fincrements
       ELSE 
         CLOSE fincrements
       END-IF

       OPEN I-O freservations
       IF fres_stat =35 THEN
              OPEN OUTPUT freservations
              CLOSE freservations    
       ELSE 
              CLOSE freservations
       END-IF

       OPEN I-O frepresentations
       IF frep_stat =35 THEN
              OPEN OUTPUT frepresentations
              CLOSE frepresentations    
       ELSE 
              CLOSE frepresentations
       END-IF

       OPEN I-O feditions
       IF fe_stat =35 THEN
              OPEN OUTPUT feditions
              CLOSE feditions    
       ELSE 
              CLOSE feditions
       END-IF
      *> Menu principal
       PERFORM WITH TEST AFTER UNTIL choixMenu=0 
         PERFORM WITH TEST AFTER UNTIL choixMenu<9                 
              DISPLAY '  _____________* Menu principal *_________'
              DISPLAY ' |Quitter le programme :                 0|'
              DISPLAY ' |Gestion des reservations :             1|'
              DISPLAY ' |Gestion des pass :                     2|'
              DISPLAY ' |Gestion des groupes :                  3|'
              DISPLAY ' |Gestion des représentations :          4|'
              DISPLAY ' |Gestion des scènes :                   5|'
              DISPLAY ' |Gestion des éditions :                 6|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choixMenu
              EVALUATE choixMenu
               WHEN 1 PERFORM GESTION_RESERVATIONS
               WHEN 2 PERFORM GESTION_PASS
               WHEN 3 PERFORM GESTION_GROUPES
               WHEN 4 PERFORM GESTION_REPRESENTATIONS
               WHEN 5 PERFORM GESTION_SCENES
               WHEN 6 PERFORM GESTION_EDITIONS
              END-EVALUATE
         END-PERFORM
       END-PERFORM
       STOP RUN.

      *> Gestion des réservations
       GESTION_RESERVATIONS.
      	PERFORM WITH TEST AFTER UNTIL choix=0 
         PERFORM WITH TEST AFTER UNTIL choix<9                 
              DISPLAY '  ______* Gestion des réservations *______'
              DISPLAY ' |Annuler :                              0|'
              DISPLAY ' |Ajouter une reservation :              1|'
              DISPLAY ' |Rechercher une reservation :           2|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_RESERVATION
              WHEN 2 PERFORM RECHERCHER_RESERVATION
              END-EVALUATE
         END-PERFORM
       END-PERFORM.

       AJOUTER_RESERVATION.
              OPEN I-O freservations 
               OPEN EXTEND fincrements
                READ fincrements
                MOVE fi_idResa TO fres_id
                 DISPLAY "Resa Avant  add : ",fi_idResa
                ADD 1 TO fi_idResa 
                DISPLAY "Resa : ",fi_idResa
                REWRITE finTampon
                END-REWRITE
               CLOSE fincrements 

              READ freservations
              MOVE 01 TO j
              MOVE 01 TO m
              MOVE 1801 TO j

              DISPLAY 'Quel est le prénom du participant?'
              ACCEPT fres_prenom
              DISPLAY 'Quelle est son département de résidence?'
              ACCEPT fres_dep
              DISPLAY'Quel est l''édition à laquelle il veut participer'
              ACCEPT fres_dateA
              DISPLAY 'Quel pass voulez vous acheter? '
              MOVE fres_dateA TO fp_dateA
              PERFORM AFFICHER_PASS_EDITION
              ACCEPT fres_nomPa
             PERFORM WITH TEST AFTER UNTIL j>00 AND j<=31 
              DISPLAY 'Quel est son jour de naissance?'
              ACCEPT j
             END-PERFORM
             PERFORM WITH TEST AFTER UNTIL m>00 AND m<=12 
              DISPLAY 'Quel est son mois de naissance?'
              ACCEPT m
             END-PERFORM
             PERFORM WITH TEST AFTER UNTIL y>1800 AND y<=2016 
              DISPLAY 'Quel est son année de naissance?'
              ACCEPT y
             END-PERFORM
              STRING j m y INTO fres_dateNaissance
              DISPLAY 'Quel est son adresse e-mail?'
              ACCEPT fres_adresseEmail
              DISPLAY 'Quel est son numéro de téléphone?'
              ACCEPT fres_numTel
              WRITE fresTampon END-WRITE
              PERFORM AFFICHER_RESERVATION
              CLOSE freservations.

       RECHERCHER_RESERVATION.
              PERFORM WITH TEST AFTER UNTIL choix=0 
               PERFORM WITH TEST AFTER UNTIL choix<9                 
                 DISPLAY '  _________* Recherche réservation*_______'
                 DISPLAY ' |Retour menu principal:                 0|'
                 DISPLAY ' |Recherche par nom :                    1|'
                 DISPLAY ' |Recherche par id :                     2|'
                 DISPLAY ' |Recherche par édition :                3|'
                 DISPLAY ' |Recherche par département :            4|'
                 DISPLAY ' |________________________________________|'
                 DISPLAY 'Faites un choix : ' WITH NO ADVANCING
                 ACCEPT choix
                 EVALUATE choix
                 WHEN 1 PERFORM RECHERCHE_RESERVATION_NOM
                 WHEN 2 PERFORM RECHERCHE_RESERVATION_ID
                 WHEN 3 PERFORM RECHERCHE_RESERVATION_EDITION
                 WHEN 4 PERFORM RECHERCHE_RESERVATION_DEP
                END-EVALUATE
               END-PERFORM
              END-PERFORM.

       RECHERCHE_RESERVATION_NOM.
              OPEN INPUT freservations 
              MOVE 0 TO Wfin
              DISPLAY 'Quel est le nom du participant?'
              ACCEPT fres_nomPa 
              MOVE fres_nomPa TO nomPa
              START freservations, KEY = fres_nomPa
                INVALID KEY 
                     DISPLAY "Il n'y a aucune réservation à ce nom."
                NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL  Wfin = 1
                     READ freservations NEXT RECORD
                     AT END MOVE 1 TO Wfin
                     NOT AT END
                      IF fres_nomPa = nomPa THEN
                      PERFORM AFFICHER_RESERVATION
                      ELSE                      
                      MOVE 1 TO Wfin
                      END-IF
                     END-READ
                  END-PERFORM
                END-START
              CLOSE freservations.

       RECHERCHE_RESERVATION_ID.
               OPEN INPUT freservations 
               DISPLAY 'Quel est l''id du participant?'
               ACCEPT fres_id 
               READ freservations
               INVALID KEY 
                     DISPLAY "Il n'y a aucune réservation à cet id."
               NOT INVALID KEY
               PERFORM AFFICHER_RESERVATION
              CLOSE freservations.

       RECHERCHE_RESERVATION_EDITION.
              OPEN I-O freservations 
              MOVE 0 TO Wfin
              DISPLAY 'Quelle édition voulez vous rechercher?'
              ACCEPT fres_dateA 
              MOVE fres_dateA  TO dateA
              START freservations, KEY = fres_dateA
                INVALID KEY 
                DISPLAY "Il n'y a aucune réservation pour cette édition."
                NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL  Wfin = 1
                     READ freservations NEXT RECORD
                     AT END MOVE 1 TO Wfin
                     NOT AT END
                      IF fres_dateA = dateA THEN
                       PERFORM AFFICHER_RESERVATION
                      ELSE  
                      MOVE 1 TO Wfin
                      END-IF
                     END-READ
                  END-PERFORM
                END-START
              CLOSE freservations.

       RECHERCHE_RESERVATION_DEP.
              OPEN INPUT freservations 
              DISPLAY 'Quelle département voulez vous rechercher?'
              MOVE 0 TO Wfin
              ACCEPT fres_dep  
              MOVE fres_dep   TO dep
              START freservations, KEY = fres_dep 
                INVALID KEY 
               DISPLAY "Il n'y a aucune réservation pour ce département."
                NOT INVALID KEY
                  PERFORM WITH TEST AFTER UNTIL  Wfin = 1
                     READ freservations NEXT RECORD
                     AT END MOVE 1 TO Wfin
                     NOT AT END
                      IF fres_dep  = dep THEN
                       PERFORM AFFICHER_RESERVATION
                      ELSE
                      MOVE 1 TO Wfin
                      END-IF
                     END-READ
                  END-PERFORM
                END-START
              CLOSE freservations.

       AFFICHER_RESERVATION.
        DISPLAY '_________________________________________'
        DISPLAY 'Id de réservation  : ', fres_id 
        DISPLAY 'Pass               : ', fres_nomPa
        DISPLAY 'Prénom             : ', fres_prenom
        DISPLAY 'Département        : ', fres_dep
        DISPLAY 'Edition            : ', fres_dateA 
        DISPLAY 'Adresse mail       : ', fres_adresseEmail
        DISPLAY 'Numéro de téléphone: ', fres_numTel
        DISPLAY 'Date de naissance  : ', fres_dateNaissance
        DISPLAY '_________________________________________'.

      *>Gestion des pass
       GESTION_PASS.
      	PERFORM WITH TEST AFTER UNTIL choix=0 
         PERFORM WITH TEST AFTER UNTIL choix<9                 
              DISPLAY '  _________* Gestion des Pass *___________'
              DISPLAY ' |Annuler :                              0|'
              DISPLAY ' |Ajouter un pass :                      1|'
              DISPLAY ' |Rechercher un pass :                   2|'
              DISPLAY ' |Modifier un pass :                     3|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_PASS
              WHEN 2 PERFORM RECHERCHER_PASS
              WHEN 3 PERFORM MODIFIER_PASS
       END-EVALUATE
       END-PERFORM
       END-PERFORM.

       AJOUTER_PASS.
       OPEN I-O fpass 
       MOVE 2000 TO fp_dateA
       PERFORM WITH TEST AFTER UNTIL fp_dateA>1999
        DISPLAY 'Pour quelle édition voulez vous ajouter un pass?'
         ACCEPT fp_dateA
       END-PERFORM
       DISPLAY 'Quelle est le nom du pass?'
       ACCEPT fp_nomPa
       READ fpass
       INVALID KEY
         DISPLAY 'Quel prix donnez vous au pass?'
         ACCEPT fp_prix
         WRITE passTampon END-WRITE
       NOT INVALID KEY
         DISPLAY 'Impossible d''ajouter ce pass, il existe déjà.'
       CLOSE fpass.

       RECHERCHER_PASS.
       PERFORM WITH TEST AFTER UNTIL choix=0 
        PERFORM WITH TEST AFTER UNTIL choix<9                 
          DISPLAY '  ________* Recherche des pass *__________'
          DISPLAY ' |Retour au menu gestion des pass :      0|'
          DISPLAY ' |Recherche par édition et nom    :      1|'
          DISPLAY ' |Recherche par édition           :      2|'
          DISPLAY ' |________________________________________|'
          DISPLAY 'Faites un choix : ' WITH NO ADVANCING
          ACCEPT choix
          EVALUATE choix
          WHEN 1 PERFORM RECHERCHE_PASS_ID
          WHEN 2 PERFORM RECHERCHE_PASS_EDITION
         END-EVALUATE
        END-PERFORM
       END-PERFORM.

       RECHERCHE_PASS_EDITION.
       DISPLAY 'Quel est l''édition?'
       ACCEPT fp_dateA 
       PERFORM AFFICHER_PASS_EDITION.
       

       AFFICHER_PASS_EDITION.
       OPEN INPUT fpass 
       MOVE 0 TO Wfin
       MOVE fp_dateA TO dateA
       START fpass, KEY = fp_dateA
         INVALID KEY 
              DISPLAY "Il n'y a aucun pass d'ajouté " 
        "pour cet edition."
         NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL  Wfin = 1
              READ fpass NEXT RECORD
              AT END MOVE 1 TO Wfin
              NOT AT END
               IF fp_dateA = dateA THEN
                 PERFORM AFFICHER_PASS
               ELSE                      
                 MOVE 1 TO Wfin
               END-IF
              END-READ
           END-PERFORM
         END-START
        CLOSE fpass.

       RECHERCHE_PASS_ID.
        OPEN I-O fpass 
        DISPLAY 'Quelle édition voulez vous rechercher un pass?'
        ACCEPT fp_dateA
        DISPLAY 'Quelle est le nom du pass?'
        ACCEPT fp_nomPa
        READ fpass
        INVALID KEY 
              DISPLAY "Il n'y a aucune pass à cet id."
        NOT INVALID KEY
         PERFORM AFFICHER_PASS
         DISPLAY 'Voulez vous modifier ce pass?'
         ACCEPT choix
         IF choix = 1 THEN
           PERFORM REECRIRE_PASS
         END-IF
         CLOSE fpass.

       MODIFIER_PASS.
        OPEN I-O fpass 
        DISPLAY 'Quelle édition voulez vous rechercher un pass?'
        ACCEPT fp_dateA
        DISPLAY 'Quelle est le nom du pass?'
        ACCEPT fp_nomPa
        READ fpass
        INVALID KEY 
              DISPLAY "Il n'y a aucune pass à cet id."
        NOT INVALID KEY
            PERFORM REECRIRE_PASS
       CLOSE fpass.

       REECRIRE_PASS.
        PERFORM CHOIX_MODIF_PASS
            EVALUATE choix
              WHEN 1 
                DELETE fpass
                DISPLAY 'Quelle est le nouveau nom du pass?'
                ACCEPT fp_nomPa
              WHEN 2 
                DELETE fpass
                DISPLAY 'Quelle est la nouvelle édition?'
                ACCEPT fp_dateA
              WHEN 3 
                DISPLAY 'Quelle est le nouveau prix?'
                ACCEPT fp_prix
            END-EVALUATE
       IF choix = 1 OR choix = 2 THEN
         WRITE passTampon
         INVALID KEY
           DISPLAY "impossible de réécrire"
         NOT INVALID KEY
           DISPLAY "Pass modifié"
         END-WRITE
       ELSE
         REWRITE passTampon
         INVALID KEY
           DISPLAY "impossible de réécrire"
         NOT INVALID KEY
           DISPLAY "Pass modifié"
         END-REWRITE
       END-IF.

       AFFICHER_PASS.
        DISPLAY '_________________________________________'
        DISPLAY 'Nom                : ', fp_nomPa
        DISPLAY 'Edition            : ', fp_dateA 
        DISPLAY 'Prix               : ', fp_prix
        DISPLAY '_________________________________________'.

       CHOIX_MODIF_PASS. 
        PERFORM WITH TEST AFTER UNTIL choix<9                 
          DISPLAY '  ________* Modification des pass *_______'
          DISPLAY ' |Annuler             :                  0|'
          DISPLAY ' |Modifier le nom     :                  1|'
          DISPLAY ' |Modifier l''édition :                  2|'
          DISPLAY ' |Modifier le prix    :                  3|'
          DISPLAY ' |________________________________________|'
          DISPLAY 'Faites un choix : ' WITH NO ADVANCING
          ACCEPT choix
        END-PERFORM.

      *>Gestion des groupes
       GESTION_GROUPES.
        PERFORM WITH TEST AFTER UNTIL choix=0
         PERFORM WITH TEST AFTER UNTIL choix<5                 
              DISPLAY '  _______* Menu gestion des groupes *_____'
              DISPLAY ' |Revenir au menu principal :            0| '
              DISPLAY ' |Ajouter un groupe         :            1|'
              DISPLAY ' |Afficher les groupes      :            2|'
              DISPLAY ' |Supprimer un groupe       :            3|'
              DISPLAY ' |Modifier un groupe        :            4|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_GROUPE
              WHEN 2 PERFORM AFFICHER_GROUPES
              WHEN 3 PERFORM SUPPRIMER_GROUPE
              WHEN 4 PERFORM MODIFIER_GROUPE
              END-EVALUATE
        END-PERFORM
       END-PERFORM.
              
       AJOUTER_GROUPE.
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                    DISPLAY 'Nom du groupe ?'
                        ACCEPT nomGr
                        PERFORM VERIF_NOM_GROUPE
              END-PERFORM
              MOVE nomGr TO fg_nom
              DISPLAY 'Style du groupe ?'
              ACCEPT fg_style
              OPEN EXTEND fgroupes
              WRITE fgTampon END-WRITE
              CLOSE fgroupes.
       
       VERIF_NOM_GROUPE.
              OPEN INPUT fgroupes
              MOVE 0 TO Wfin
              MOVE 0 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve = 1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                     DISPLAY 'Groupe inexistant'
                NOT AT END
                IF fg_nom = nomGr THEN
                      MOVE 1 TO Wtrouve
                     DISPLAY 'Groupe trouvé'      
                END-IF
              END-READ
              END-PERFORM
              CLOSE fgroupes.
       
        AFFICHER_GROUPES.
              OPEN INPUT fgroupes
              MOVE 0 TO Wfin
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                  DISPLAY 'fin'
                NOT AT END
                  DISPLAY 'Nom : ',fg_nom,'Style : ',fg_style
              END-READ
              END-PERFORM
              CLOSE fgroupes.

       SUPPRIMER_GROUPE.
              OPEN INPUT fgroupes
              OPEN OUTPUT fgroupesTemp
              DISPLAY 'Nom du groupe ?'
              ACCEPT nomGr
              MOVE 0 TO Wfin
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                NOT AT END
                IF fg_nom = nomGr THEN
                  DISPLAY 'groupe trouvé'
                ELSE 
                  MOVE fgTampon to fgTamponTemp
                  WRITE fgTamponTemp END-WRITE
                END-IF
              END-READ
              END-PERFORM
              CLOSE fgroupesTemp
              CLOSE fgroupes
              OPEN OUTPUT fgroupes
              OPEN INPUT fgroupesTemp
              MOVE 0 TO Wfin
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fgroupesTemp
                AT END MOVE 1 TO Wfin
                NOT AT END
                  MOVE fgTamponTemp to fgTampon
                  WRITE fgTampon END-WRITE
              END-READ
              END-PERFORM
              CLOSE fgroupesTemp
              CLOSE fgroupes.

       MODIFIER_GROUPE.
              OPEN INPUT fgroupes
              DISPLAY 'Nom du groupe ?'
              ACCEPT nomGr
              MOVE 0 TO Wfin
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve=1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                NOT AT END
                IF fg_nom = nomGr THEN
                  MOVE 1 TO Wtrouve
                  DISPLAY 'trouvé'
                END-IF   
              END-READ
              END-PERFORM
              CLOSE fgroupes
              IF Wtrouve = 1
                OPEN I-O fgroupes
                MOVE 0 to choixMenu
              PERFORM WITH TEST AFTER UNTIL choixMenu>0 OR choixMenu<=2                 
                DISPLAY 'Que voulez-vous modifier ?'
                DISPLAY '1- Le nom'
                DISPLAY '2- Le style'
              ACCEPT choixMenu
              EVALUATE choixMenu
              WHEN 1 
                DISPLAY 'Nouveau nom = '
                ACCEPT fg_nom
              WHEN 2 
                DISPLAY 'Nouveau style = '
                ACCEPT fg_style
              END-EVALUATE
              END-PERFORM
              DISPLAY fgTampon
              REWRITE fgTampon END-REWRITE
              DISPLAY 'groupe modifié'
              CLOSE fgroupes
              ELSE
                DISPLAY 'le groupe n existe pas'
              END-IF.
      *>Gestion des représentations
       GESTION_REPRESENTATIONS.
              PERFORM WITH TEST AFTER UNTIL choix=0
         PERFORM WITH TEST AFTER UNTIL choix<5                 
              DISPLAY '  __* Menu gestion des représentation *___'
              DISPLAY ' |Revenir au menu principal :            0| '
              DISPLAY ' |Ajouter une nouvelle représentation :  1|'
              DISPLAY ' |Afficher les représentations par année:2|'
              DISPLAY ' |Supprimer une représentation :         3|'
              DISPLAY ' |Modifier  une représentation :         4|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_NOUVELLE_REPRESENTATION
              WHEN 2 PERFORM AFFICHER_REPRESENTATION
              WHEN 3 PERFORM SUPPRIMER_REPRESENTATION
              WHEN 4 PERFORM MODIFIER_REPRESENTATION
       END-EVALUATE
       END-PERFORM
       END-PERFORM.

       AJOUTER_NOUVELLE_REPRESENTATION.
              OPEN INPUT frepresentations 
              READ frepresentations
              INVALID KEY
              CLOSE frepresentations
              OPEN I-O frepresentations
      *>PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
              DISPLAY 'Année ?'
              ACCEPT frep_dateA
                OPEN INPUT feditions
                MOVE 0 TO Wfin
                MOVE 0 TO Wtrouve
                PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve = 1
                READ feditions
                 AT END MOVE 1 TO Wfin
                   DISPLAY 'Pas d''édition cette année'
                 NOT AT END
                  IF fe_dateA = frep_dateA THEN
                    MOVE 1 TO Wtrouve
                    DISPLAY 'L''année est valide'      
                  END-IF
                END-READ
              END-PERFORM
              CLOSE feditions
      *>END-PERFORM
              PERFORM WITH TEST AFTER UNTIL frep_jour <= 3
                DISPLAY 'Jour de la représentation ?'
                ACCEPT frep_jour
              END-PERFORM
              PERFORM WITH TEST AFTER UNTIL frep_heureDebut >= 0000 AND frep_heureDebut < 2400
                DISPLAY 'Heure de début ? '
                ACCEPT frep_heureDebut
              END-PERFORM
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                DISPLAY 'Nom du groupe ?'
                ACCEPT frep_nomGr
              PERFORM VERIF_NOM_GROUPE
              END-PERFORM
              MOVE nomGr TO frep_nomSce
              DISPLAY 'Nom de la scène ?'
              ACCEPT frep_nomSce
              PERFORM WITH TEST AFTER UNTIL frep_cachet GREATER 0
                DISPLAY 'Cachet ?'
                ACCEPT frep_cachet
              END-PERFORM
              PERFORM WITH TEST AFTER UNTIL frep_nbPersonneMax GREATER 0
                DISPLAY 'Nombre de personne max ?'
                ACCEPT frep_nbPersonneMax
              END-PERFORM
              WRITE frepTampon END-WRITE
              NOT INVALID KEY
                DISPLAY 'La représentation existe déjà'
              CLOSE frepresentations.

        AFFICHER_REPRESENTATION.
              OPEN I-O frepresentations
              READ frepresentations
              INVALID KEY                           
                DISPLAY 'Année de l édition ?'
                ACCEPT frep_dateA 
                START frepresentations,
                KEY = frep_dateA
                  INVALID KEY
                    DISPLAY 'Pas d édition cette année'
                    MOVE 1 TO Wfin
                  NOT INVALID KEY
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                  READ frepresentations NEXT RECORD
                  AT END
                    DISPLAY "C'est tout!"
                    MOVE 1 TO Wfin
                  NOT AT END
                    DISPLAY 'Le groupe ',frep_nomGr,'joue sur 'frep_nomSce,'le ',frep_jour,' à ',frep_heureDebut
                  END-READ
                END-PERFORM
                 END-START
                CLOSE frepresentations.

       SUPPRIMER_REPRESENTATION.
              OPEN I-O frepresentations
                     DISPLAY 'Nom de la scène :'
                     ACCEPT frep_nomSce
                     DISPLAY 'Année édition :'
                     ACCEPT frep_dateA
                     DISPLAY 'Jour :'
                     ACCEPT frep_jour
                     DISPLAY 'Heure :'
                     ACCEPT frep_heureDebut
                     DELETE frepresentations RECORD
                            INVALID KEY
                                   DISPLAY 'La représentation n existe pas'
                            NOT INVALID KEY
                                   DISPLAY 'Représentation supprimée'
              CLOSE frepresentations.
       
       MODIFIER_REPRESENTATION.
          OPEN I-O frepresentations
          IF fres_stat =35 THEN
             DISPLAY 'Pas de représentation'
          ELSE    
             DISPLAY 'Clé de la représentation : '
                     DISPLAY 'Nom de la scène :'
                     ACCEPT frep_nomSce
                     DISPLAY 'Année édition :'
                     ACCEPT frep_dateA
                     DISPLAY 'Jour :'
                     ACCEPT frep_jour
                     DISPLAY 'Heure :'
                     ACCEPT frep_heureDebut
              READ frepresentations
             INVALID KEY
               DISPLAY 'La représentation n existe pas'
             NOT INVALID KEY
                PERFORM WITH TEST AFTER UNTIL choixModifReserv < 1
                   DISPLAY ' _____* Modification représentation *____'
                   DISPLAY '| Annuler                   :           0|'
                   DISPLAY '| Le jour                   :           1|'
                   DISPLAY '| Heure de début            :           2|'      
                   DISPLAY '| Le nom du groupe          :           3|'
                   DISPLAY '| Le cachet                 :           4|'
                   DISPLAY '| Le nombre de personne max :           5|'
                   DISPLAY '|________________________________________|'
                   DISPLAY 'Faites un choix : ' WITH NO ADVANCING
                   ACCEPT  choixModifReserv
                   EVALUATE  choixModifReserv
                   WHEN 1 
                      PERFORM WITH TEST AFTER UNTIL frep_jour >= 01 AND frep_jour <= 03
                         DISPLAY 'Donnez un jour (01, 02, 03)'
                         ACCEPT frep_jour
                         REWRITE frepTampon END-REWRITE
                      END-PERFORM 
                   WHEN 2 
                      PERFORM WITH TEST AFTER UNTIL frep_heureDebut >= 0000 
                       AND frep_heureDebut <2400
                       DISPLAY frep_heureDebut
                         DISPLAY 'Donnez une heure au format (HHMM)'
                         ACCEPT frep_heureDebut
                         DISPLAY frep_nomSce
                         DISPLAY frep_dateA
                         DISPLAY frep_jour
                         DISPLAY frep_heureDebut
                         REWRITE frepTampon
                            INVALID KEY 
                            DISPLAY "*** ERREUR INTERNE (rewrite)"
                            NOT INVALID KEY
                            DISPLAY "ok."
                         END-REWRITE
                      END-PERFORM 
                   WHEN 3 
                     MOVE 0 TO Wtrouve
                     PERFORM WITH TEST AFTER UNTIL Wtrouve = 0
                            DISPLAY 'Donnez le nom du groupe'
                            ACCEPT frep_nomGr
                     PERFORM VERIF_NOM_GROUPE
                     END-PERFORM
                   WHEN 4 
                      PERFORM WITH TEST AFTER UNTIL frep_cachet > 0
                      END-PERFORM
                    WHEN 5 
                      PERFORM WITH TEST AFTER UNTIL frep_nbPersonneMax > 0
                         DISPLAY 'Donnez le nouveau cachet'
                         ACCEPT frep_nbPersonneMax
                         REWRITE frepTampon END-REWRITE
                      END-PERFORM
                   END-EVALUATE
                END-PERFORM
                END-READ
          CLOSE frepresentations
        END-IF.

        *>Gestion des scenes
          GESTION_SCENES.
         PERFORM WITH TEST AFTER UNTIL choix = 0
          PERFORM WITH TEST AFTER UNTIL choix< 9    
              DISPLAY " _______* Menu gestion des scènes *_______ "
              DISPLAY "|Annuler                  :              0|"
              DISPLAY "|Ajouter scene            :              1|"
              DISPLAY "|Afficher scene / edition :              2|"
              DISPLAY "|Supprimer scene          :              3|"
              DISPLAY "|Mofifier scene           :              4|"
              DISPLAY "|_________________________________________|"
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING

              ACCEPT choix
       
              EVALUATE choix
               WHEN 1 PERFORM AJOUT_SCENES
               WHEN 2 PERFORM AFFICHER_SCENES_ANNEE
               WHEN 3 PERFORM SUPPRIMER_SCENE
               WHEN 4 PERFORM MODIFIER_SCENE
              END-EVALUATE

              IF (choix <0) OR (choix) > 9 THEN 
                DISPLAY "Attention saisir une valeur correcte"
              END-IF
           END-PERFORM    
          END-PERFORM.    

       VERIF_SCENES. 
       OPEN INPUT fscenes
       MOVE 0 TO Wtrouve

       DISPLAY 'Saisir nom de la scene :'
       ACCEPT fs_nomSce

       PERFORM WITH TEST AFTER UNTIL fs_dateA > 1000
        DISPLAY 'Saisir Annee au format YYYY ' 
        ACCEPT fs_dateA
       END-PERFORM  
       
       READ fscenes
        INVALID KEY 
         DISPLAY 'La scene n''existe pas '
        NOT INVALID KEY 
         MOVE 1 TO Wtrouve
         DISPLAY 'La scene est présente' 
       END-READ 

       CLOSE fscenes.


       SUPPRIMER_SCENE. 
       PERFORM VERIF_SCENES
       MOVE 0 TO WrepSc
       IF Wtrouve = 0 
        DISPLAY 'La scene spécifié n''existe pas' 
       ELSE 
         PERFORM VERIF_PROGRAMME_SCENE 
        IF Wprog = 1 
          DISPLAY "Impossible de supprimer la scene " 
          DISPLAY "Représentation programmé !"
        ELSE 
          OPEN I-O fscenes
            DELETE fscenes RECORD
            INVALID KEY 
              DISPLAY "Impossible de supprimer "
            NOT INVALID KEY 
              DISPLAY "Supprimer"  
          CLOSE fscenes
        END-IF
       END-IF. 

       VERIF_PROGRAMME_SCENE. 
       MOVE 0 TO Wprog
       MOVE fs_nomSce TO frep_nomSce 
       OPEN INPUT frepresentations
       START frepresentations, KEY = frep_nomSce

        INVALID KEY 
           DISPLAY 'Aucune représentation programmé' 
        NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL WFin = 1 OR Wprog = 1
            READ frepresentations NEXT 
            AT END 
              MOVE 1 TO WFin
              DISPLAY "Fin "
           NOT AT END   
              IF frep_dateA = fs_dateA
                MOVE 1 TO Wprog
              END-IF
          END-PERFORM  
       CLOSE frepresentations.


       MODIFIER_SCENE.
       PERFORM AFFICHER_SCENES_ANNEE
       IF wTrouve = 1 
        PERFORM VERIF_SCENES
        IF Wtrouve = 0 
          DISPLAY "modification impossible"
        ELSE 
         PERFORM VERIF_PROGRAMME_SCENE    
         IF Wprog = 1
           DISPLAY 'Il y a une représentation programmée sur la scène'
         ELSE 
           OPEN I-O fscenes
           MOVE 1 TO choix 
           PERFORM WITH TEST AFTER UNTIL choix= 0
              DISPLAY " ______* Modification des scènes *________"
              DISPLAY "|Annuler              :                  0|"
              DISPLAY "|Modifier capicité    :                  1|"
              DISPLAY "|Modifier cout        :                  2|"
              DISPLAY "|_________________________________________|"
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING

              ACCEPT choix
       
              EVALUATE choix
               WHEN 1 PERFORM MODIFIER_SCENE_CAPACITE
               WHEN 2 PERFORM MODIFIER_SCENE_COUT

              END-EVALUATE

              IF (choix <0) OR (choix) > 2 THEN 
                DISPLAY "Attention saisir une valeur correcte"
              END-IF
           END-PERFORM   
           REWRITE fscTampon  
            INVALID KEY DISPLAY 'Scène non enregistré'
            NOT INVALID KEY DISPLAY 'Scene enregistré'
           END-REWRITE 
 
           PERFORM AFFICHER_SCENES
           CLOSE fscenes
         END-IF
          DISPLAY "modification impossible"
        END-IF 
       END-IF.


       AFFICHER_SCENES.
       DISPLAY '_________________________________________'
       DISPLAY 'Nom:      : ' ,fs_nomSce
       DISPLAY 'Année     : ' ,fs_dateA
       DISPLAY 'capcicité : ' ,fs_capacite
       DISPLAY 'Cout      : ' ,fs_cout
       DISPLAY '_________________________________________'.

       MODIFIER_SCENE_CAPACITE.
       MOVE 0 TO fs_capacite
       PERFORM WITH TEST AFTER UNTIL fs_capacite > 0 AND fs_capacite<99
            DISPLAY 'Saisir une capacite supérieure à 0 et inférieure à 99'
            ACCEPT fs_capacite
       END-PERFORM.  

       MODIFIER_SCENE_COUT.
       MOVE 0 TO fs_cout
       PERFORM WITH TEST AFTER UNTIL fs_cout > 0
         DISPLAY 'Saisir un cout supérieure à 0'
         ACCEPT fs_cout
       END-PERFORM.  

       AJOUT_SCENES.
       DISPLAY "Saisir l'année"
       ACCEPT fe_dateA
       OPEN INPUT feditions 
       PERFORM VERIF_EDITION
       CLOSE feditions

       IF Wtrouve = 1
        PERFORM VERIF_SCENES
        IF Wtrouve = 0 
        OPEN I-O fscenes

        PERFORM MODIFIER_SCENE_COUT
        PERFORM MODIFIER_SCENE_CAPACITE
      
        WRITE fscTampon  
          INVALID KEY DISPLAY 'Scène non enregistré'
          NOT INVALID KEY DISPLAY 'Scene enregistré'
        END-WRITE 
 
        PERFORM AFFICHER_SCENES
        CLOSE fscenes
        ELSE 
          DISPLAY 'La scène existe déjà'
        END-IF
       ELSE 
       
       DISPLAY "Edition inconnue, vérifier qu'une édition à été créée"
       " pour l'année spécifiée"

       END-IF.

         
       AFFICHER_SCENES_ANNEE.
       MOVE 0 TO WFin
       DISPLAY 'Saisir l''année de l''édition' 

       PERFORM WITH TEST AFTER UNTIL fs_dateA > 1000
          DISPLAY 'Saisir Annee au format YYYY ' 
          ACCEPT fs_dateA
       END-PERFORM  

       OPEN INPUT fscenes

       START fscenes, KEY = fs_dateA

        INVALID KEY 
           DISPLAY 'Pas de scène pour l''année saisie' 
        NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL WFin = 1
            READ fscenes NEXT 
            AT END 
              MOVE 1 TO WFin
              DISPLAY "Fin "
           NOT AT END   
              PERFORM AFFICHER_SCENES
          END-PERFORM  
       CLOSE fscenes
       DISPLAY 'over'.

      *> Gestion editions

       GESTION_EDITIONS.
       PERFORM WITH TEST AFTER UNTIL choixMenu=0
         PERFORM WITH TEST AFTER UNTIL choixMenu<9                 
           DISPLAY "  ____* Menu gestion des éditions *_______"
           DISPLAY " |Afficher les éditions :                0|"
           DISPLAY " |Ajout d'une éditions :                 1|"
           DISPLAY " |Modifier la capacité d'une édition :   2|"
           DISPLAY " |Modifier le nombre de jour              |"
           DISPLAY " |                       d'une édition : 3|"
           DISPLAY " |________________________________________|"
           DISPLAY 'Faites un choix : ' WITH NO ADVANCING

           ACCEPT choixMenu
           EVALUATE choixMenu
             WHEN 1 PERFORM AFFICHER_EDITIONS
             WHEN 2 PERFORM AJOUT_EDITIONS
             WHEN 3 PERFORM MODIFIER_CAPACITE
           END-EVALUATE
         END-PERFORM
       END-PERFORM.

       AFFICHER_EDITIONS.
       DISPLAY "Affichage des éditions du festival"
       DISPLAY "___________________________________________"
       OPEN I-O feditions
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin=1
         READ feditions NEXT
           AT END
             MOVE 1 TO Wfin
           NOT AT END
             DISPLAY "___________________________________________"
             DISPLAY "Edition ",fe_dateA
             DISPLAY "Capacité : ",fe_capacite
             DISPLAY "Nombre de scènes : ",fe_nbScene
             DISPLAY "Nombre d'artistes : ",fe_nbArtiste
             DISPLAY "Nombre de réservation jour 1 : ",fe_nbResaJourUn
             DISPLAY "Nombre de réservation jour 2 : ",fe_nbResaJourDeux
             DISPLAY "Nombre de réservation jour 2 : ",fe_nbResaJourTrois
             DISPLAY "Résultat final : ",fe_resultat," euros"
             DISPLAY "Coût moyen d'une scène : ",fe_coutMoyenScene
             DISPLAY "Cachet moyen : ",fe_coutArtistes
         END-READ
       END-PERFORM
       CLOSE feditions.

       AJOUT_EDITIONS.
       DISPLAY "Ajout d'une édition"
       DISPLAY "___________________________________________"
       OPEN I-O feditions
       MOVE 0 TO Wtrouve
       DISPLAY "Veuillez renseigner les informations suivantes :"
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
         DISPLAY "Indiquez l'annee de l'édition : " WITH NO ADVANCING
         ACCEPT fe_dateA
         READ feditions
         INVALID KEY
           PERFORM WITH TEST AFTER UNTIL fe_capacite IS NUMERIC AND
       fe_capacite > 0
             DISPLAY "Indiquez la capacité de l'édition : " WITH NO 
        ADVANCING
             ACCEPT fe_capacite
           END-PERFORM
           PERFORM WITH TEST AFTER UNTIL fe_nbJour > 1 AND fe_nbJour < 4
       AND fe_nbJour IS NUMERIC
             DISPLAY "Indiquez la durée du festival : " WITH NO ADVANCING
             ACCEPT fe_nbJour
           END-PERFORM
           MOVE 0 TO fe_nbScene
           MOVE 0 TO fe_nbArtiste
           MOVE 0 TO fe_nbResaJourUn
           MOVE 0 TO fe_nbResaJourDeux
           MOVE 0 TO fe_nbResaJourTrois
           MOVE 0 TO fe_resultat
           MOVE 0 TO fe_coutMoyenScene
           MOVE 0 TO fe_coutArtistes
           WRITE fedTampon
           MOVE 1 TO Wtrouve
        NOT INVALID KEY
           DISPLAY "Il y a déjà une édition enregistrée pour cette date."
         END-READ
       END-PERFORM
       CLOSE feditions.

       MODIFIER_CAPACITE.
       DISPLAY "*********"
       OPEN I-O feditions
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
         DISPLAY "Veuillez saisir l'année de l'édition : "
         ACCEPT fe_dateA
         READ feditions
         INVALID KEY
           DISPLAY "Aucune édition à cette date."
         NOT INVALID KEY
           PERFORM WITH TEST AFTER UNTIL fe_capacite > 0 AND fe_capacite
       IS NUMERIC
             DISPLAY "Veuillez saisir la capacité : " WITH NO ADVANCING
             ACCEPT fe_capacite
             IF fe_capacite < 0 THEN
               DISPLAY "La capacité n'est pas correcte."
             END-IF
           END-PERFORM
           REWRITE fedTampon
           MOVE 1 to Wtrouve
         END-READ
       END-PERFORM
       CLOSE feditions.
       
       VERIF_EDITION.
       READ feditions
       INVALID KEY 
         MOVE 0 TO Wtrouve
       NOT INVALID KEY
         MOVE 1 TO Wtrouve
       END-READ.
       END PROGRAM projet.
