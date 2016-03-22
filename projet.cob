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
          02 fres_nomPa         PIC 9(3).
          02 fres_prenom        PIC A(30).
          02 fres_dep           PIC 9(2).
          02 fres_dateA         PIC 9(4).
          02 fres_adresseEmail  PIC X(30).
          02 fres_numTel        PIC XXXXXXXXXX.
          02 fres_dateNaissance PIC 9(8). 

        FD fpass.
        01 fpassTampon.
          02 fp_clePass. 
            03 fp_nomPa PIC 9(3).
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
          02 fe_resultat PIC S9(30). 
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
        77 Wcount PIC 9(3).
        77 Wallowed PIC 9(1).
      *> Variables globales   
        77 choixMenu PIC 9(2).
        77 choix     PIC 9(2).
        77 Wfin      PIC 9.

    	*>Variables pass réservation
        77 nomPa     PIC 9(3).
        77 dateA     PIC 9(4).
        77 dep       PIC 9(2).
        77 j         PIC 99.
        77 m         PIC 99.
        77 y         PIC 9999.
        77 Wtrouve   PIC 9(1).
        77 Wprix     PIC 9(4).
    	*>Variables groupe représentation
        77 nomGr PIC A(30).
        77 styleGr PIC A(30).
        77 pos PIC 9.
        77 posFin PIC 9.
        77 nomDernier PIC A(30).
        77 styleDernier PIC A(30).
        77 choixModifReserv PIC 9(2).
    	*>VARIABLES SCENE 
        77 WnbScene PIC 9(2).
        77 WResTemp PIC S9(30).
        77 WCouTemp PIC 9(30).


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
     *>  Statistiques
        77 nbArtisteN PIC 9(3).
        77 nbMoyArtiste PIC 9(3).
        77 nbEdition PIC 9(3).

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
              DISPLAY ' |Quitter le programme        :          0|'
              DISPLAY ' |Gestion des reservations    :          1|'
              DISPLAY ' |Gestion des pass            :          2|'
              DISPLAY ' |Gestion des groupes         :          3|'
              DISPLAY ' |Gestion des représentations :          4|'
              DISPLAY ' |Gestion des scènes          :          5|'
              DISPLAY ' |Gestion des éditions        :          6|'
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
              DISPLAY ' |Annuler                    :           0|'
              DISPLAY ' |Ajouter une reservation    :           1|'
              DISPLAY ' |Rechercher une reservation :           2|'
              DISPLAY ' |Modifier une reservation   :           3|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_RESERVATION
              WHEN 2 PERFORM RECHERCHER_RESERVATION
              WHEN 3 PERFORM MODIFIER_RESERVATION
              END-EVALUATE
         END-PERFORM
       END-PERFORM.

       AJOUTER_RESERVATION.
              OPEN I-O freservations 
               OPEN INPUT fincrements
                READ fincrements
                MOVE fi_idResa TO fres_id
                 *>DISPLAY "Resa Avant  add : ",fi_idResa
                ADD 1 TO fi_idResa 
                CLOSE fincrements 
                OPEN OUTPUT fincrements
               *>DISPLAY "Resa : ",fi_idResa
                WRITE finTampon
                END-WRITE
               	CLOSE fincrements

              READ freservations
              MOVE 01 TO j
              MOVE 01 TO m
              MOVE 1801 TO j


              DISPLAY'Quel est l''édition à laquelle il veut participer'
              ACCEPT fres_dateA
              MOVE fres_dateA TO fp_dateA
              PERFORM AFFICHER_PASS_EDITION
              IF Wtrouve = 1 THEN
                MOVE fres_dateA TO fe_dateA
                PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 AND Wallowed = 1
                  DISPLAY 'Quel pass voulez vous acheter? '
                  ACCEPT fres_nomPa
                  MOVE fres_nomPa TO fp_nomPa
                  PERFORM VERIF_PASS_ID
                  PERFORM VERIF_PASS_DISPO
                  IF Wallowed = 0 THEN
                    DISPLAY 'Il n''a plus de place pour l''un des jours'
                    'séléctionné'
                END-PERFORM

                DISPLAY 'Quel est le prénom du participant?'
                ACCEPT fres_prenom
                DISPLAY 'Quelle est son département de résidence?'
                ACCEPT fres_dep
              
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

                PERFORM WITH TEST AFTER UNTIL Wcount > 0
                  MOVE 0 TO Wcount
                  DISPLAY 'Quel est son adresse e-mail?'
                  ACCEPT fres_adresseEmail
                  INSPECT fres_adresseEmail TALLYING Wcount FOR CHARACTERS  AFTER INITIAL '@'
                END-PERFORM
  	            
                PERFORM WITH TEST AFTER UNTIL Wcount = 0
                  MOVE 0 TO Wcount
  	              DISPLAY 'Quel est son numéro de téléphone?'
  	              ACCEPT fres_numTel
                  INSPECT fres_numTel TALLYING Wcount FOR ALL SPACES
                END-PERFORM
                MOVE fres_dateA TO fe_dateA 
  	            WRITE fresTampon 
                NOT INVALID KEY
                  PERFORM MAJ_NBRESERVATION
                  DISPLAY 'labite'
                END-WRITE
              END-IF
              CLOSE freservations.

       MODIFIER_RESERVATION.
              PERFORM WITH TEST AFTER UNTIL choix=0 OR choix = 1 
                DISPLAY 'Nous allons vous demander l''identifiant du'
                ' participant. Désirez-vous rechercher un participant'
                ' avant de continuer?'
                DISPLAY '0 pour non, 1 pour oui : ' WITH NO ADVANCING
                ACCEPT choix
              END-PERFORM
              IF choix = 1 THEN 
                PERFORM RECHERCHER_RESERVATION
              END-IF
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
                PERFORM RECHERCHE_RESERVATION_ID
                PERFORM WITH TEST AFTER UNTIL choix=0 
                 PERFORM WITH TEST AFTER UNTIL choix<9                 
                   DISPLAY '  _________* Recherche réservation*_______'
                   DISPLAY ' |Annuler                   :            0| '
                   DISPLAY ' |Modifier le nom           :            1|'
                   DISPLAY ' |Modifier l''adresse mail  :            2|'
                   DISPLAY ' |Modifier le numéro de tél :            3|'
                   DISPLAY ' |________________________________________|'
                   DISPLAY 'Faites un choix : ' WITH NO ADVANCING
                   ACCEPT choix
                   EVALUATE choix
                   WHEN 1 PERFORM MODIFIER_RESERVATION_NOM
                   WHEN 2 PERFORM MODIFIER_RESERVATION_MAIL
                   WHEN 3 PERFORM MODIFIER_RESERVATION_TEL
                  END-EVALUATE
                 END-PERFORM
                 END-PERFORM
              END-PERFORM.

       MODIFIER_RESERVATION_NOM.
              OPEN I-O freservations 
               READ freservations
               INVALID KEY 
               DISPLAY "Erreur: le tampon a été altéré."
               NOT INVALID KEY
                DISPLAY 'Quelle est le nom?'
                ACCEPT fres_prenom
                REWRITE fresTampon
               INVALID KEY
                 DISPLAY "impossible de réécrire"
               NOT INVALID KEY
                 DISPLAY "Reservation modifiée"
               END-REWRITE
              CLOSE freservations.
       MODIFIER_RESERVATION_MAIL.
              OPEN I-O freservations 
               READ freservations
               INVALID KEY 
               DISPLAY "Erreur: le tampon a été altéré."
               NOT INVALID KEY
                PERFORM WITH TEST AFTER UNTIL Wcount > 0
                  MOVE 0 TO Wcount
                  DISPLAY 'Quel est la nouvelle adresse e-mail?'
                  ACCEPT fres_adresseEmail
                  INSPECT fres_adresseEmail TALLYING Wcount FOR CHARACTERS  AFTER INITIAL '@'
                END-PERFORM
                REWRITE fresTampon
               INVALID KEY
                 DISPLAY "impossible de réécrire"
               NOT INVALID KEY
                 DISPLAY "Reservation modifiée"
               END-REWRITE
              CLOSE freservations.
       
       MODIFIER_RESERVATION_TEL.
              OPEN I-O freservations 
               READ freservations
               INVALID KEY 
               DISPLAY "Erreur: le tampon a été altéré."
               NOT INVALID KEY
                PERFORM WITH TEST AFTER UNTIL Wcount = 0
                  MOVE 0 TO Wcount
                  DISPLAY 'Quel est son numéro de téléphone?'
                  ACCEPT fres_numTel
                  INSPECT fres_numTel TALLYING Wcount FOR ALL SPACES
                END-PERFORM
                REWRITE fresTampon
               INVALID KEY
                 DISPLAY "impossible de réécrire"
               NOT INVALID KEY
                 DISPLAY "Reservation modifiée"
               END-REWRITE
              CLOSE freservations.

       MAJ_NBRESERVATION.
              OPEN I-O feditions 
               READ feditions
               INVALID KEY 
               DISPLAY "Erreur: le tampon a été altéré."
               NOT INVALID KEY
               DISPLAY fres_nomPa
                EVALUATE fres_nomPa
                  WHEN 1 COMPUTE fe_nbResaJourUn = fe_nbResaJourUn + 1
                  WHEN 2 COMPUTE fe_nbResaJourDeux = fe_nbResaJourDeux + 1
                  WHEN 3 COMPUTE fe_nbResaJourTrois = fe_nbResaJourTrois + 1
                  WHEN 12  
                    COMPUTE fe_nbResaJourDeux = fe_nbResaJourDeux + 1
                  WHEN 13  
                    COMPUTE fe_nbResaJourUn = fe_nbResaJourUn - 1
                    COMPUTE fe_nbResaJourTrois = fe_nbResaJourTrois + 1
                  WHEN 23  
                    COMPUTE fe_nbResaJourDeux = fe_nbResaJourDeux + 1
                    COMPUTE fe_nbResaJourTrois = fe_nbResaJourTrois + 1
                  WHEN 123  
                    COMPUTE fe_nbResaJourUn = fe_nbResaJourUn + 1
                    COMPUTE fe_nbResaJourDeux = fe_nbResaJourDeux + 1
                    COMPUTE fe_nbResaJourTrois = fe_nbResaJourTrois + 1
                END-EVALUATE
                COMPUTE fe_resultat = fe_resultat + fp_prix
                REWRITE fedTampon
                 INVALID KEY
                   DISPLAY "impossible d''ajouter une réservation"
                 NOT INVALID KEY
                   DISPLAY "Reservation effectuée"
               END-REWRITE

              CLOSE feditions.
       
       VERIF_PASS_DISPO.
              OPEN INPUT feditions 
               READ feditions
               INVALID KEY 
               DISPLAY "Erreur: le tampon a été altéré."
               NOT INVALID KEY
                  MOVE 1 TO Wallowed 
                  EVALUATE fres_nomPa
                  WHEN 1 
                    IF fe_nbResaJourUn = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF
                  WHEN 2
                    IF fe_nbResaJourDeux = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF
                  WHEN 3
                    IF fe_nbResaJourTrois = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF
                  WHEN 12 
                    IF fe_nbResaJourDeux = fe_capacite OR fe_nbResaJourUn = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF 
                  WHEN 13  
                    IF fe_nbResaJourUn = fe_capacite OR fe_nbResaJourTrois = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF 
                  WHEN 23  
                    IF fe_nbResaJourDeux = fe_capacite OR fe_nbResaJourTrois = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF 
                  WHEN 123  
                    IF fe_nbResaJourUn = fe_capacite OR fe_nbResaJourDeux = fe_capacite OR fe_nbResaJourTrois = fe_capacite THEN 
                      MOVE 0 TO Wallowed 
                    END-IF 

                END-EVALUATE


              CLOSE feditions.

      RECHERCHER_RESERVATION.
              PERFORM WITH TEST AFTER UNTIL choix=0 
               PERFORM WITH TEST AFTER UNTIL choix<9                 
                 DISPLAY '  _________* Recherche réservation*_______'
                 DISPLAY ' |Annuler                   :            0| '
                 DISPLAY ' |Recherche par pass        :            1|'
                 DISPLAY ' |Recherche par id          :            2|'
                 DISPLAY ' |Recherche par édition     :            3|'
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
              PERFORM RECHERCHE_PASS_EDITION
              DISPLAY 'Pour quel pass voulez vous afficher les participant?'
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
                          IF fres_dateA = fp_dateA THEN 
                            PERFORM AFFICHER_RESERVATION
                          END-IF
                      ELSE                      
                      MOVE 1 TO Wfin
                      END-IF
                     END-READ
                  END-PERFORM
                END-START
              CLOSE freservations.

       RECHERCHE_RESERVATION_ID.
               MOVE 0 TO Wtrouve
               OPEN INPUT freservations 
               DISPLAY 'Quel est l''id du participant?'
               ACCEPT fres_id 
               READ freservations
               INVALID KEY 
               DISPLAY "Il n'y a aucune réservation à cet id."
               NOT INVALID KEY
               MOVE 1 TO Wtrouve
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
              DISPLAY ' |Revenir au menu principal :            0| '
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
         WRITE fpassTampon END-WRITE
       NOT INVALID KEY
         DISPLAY 'Impossible d''ajouter ce pass, il existe déjà.'
       CLOSE fpass.

       GENERER_PASS.
        OPEN I-O fpass 

        DISPLAY 'Quel prix donnez vous au pass premier jour?'
        MOVE 1 TO fp_nomPa
        ACCEPT fp_prix
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass deuxième jour?'
        ACCEPT fp_prix
        MOVE 2 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass troisième jour?'
        ACCEPT fp_prix
        MOVE 3 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass premier et deuxième jour?'
        ACCEPT fp_prix
        MOVE 12 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass deuxième et troisième jour?'
        ACCEPT fp_prix
        MOVE 23 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass premier et troisième jour?'
        ACCEPT fp_prix
        MOVE 13 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Quel prix donnez vous au pass trois jours?'
        ACCEPT fp_prix
        MOVE 123 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        CLOSE fpass.

        VERIF_FORMAT_PRIX.
          PERFORM WITH TEST BEFORE UNTIL fp_prix > 0
          DISPLAY 'Veuillez saisir une valeur numérique'
          ACCEPT fp_prix
          END-PERFORM.

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
       MOVE 0 TO Wfin
       MOVE 0 TO Wtrouve
       MOVE fp_dateA TO fe_dateA
       OPEN INPUT feditions
       PERFORM VERIF_EDITION
       CLOSE feditions
       IF Wtrouve = 1 THEN
         MOVE 0 TO Wtrouve
         OPEN INPUT fpass 
         MOVE fp_dateA TO dateA
         START fpass, KEY = fp_dateA
           INVALID KEY 
                DISPLAY "Il n'y a aucun pass d'ajouté " 
          "pour cet edition."
           NOT INVALID KEY
           MOVE 1 TO Wtrouve
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
           CLOSE fpass
        ELSE
          DISPLAY 'Edition pas trouvée'
        END-IF.

        VERIF_PASS_ID.
        OPEN INPUT fpass 
        MOVE 0 TO Wfin
        MOVE 0 TO Wtrouve
        READ fpass
         INVALID KEY 
              DISPLAY "Ce pass n'est pas valide"
         NOT INVALID KEY
         	  DISPLAY "Pass correcte"
         	  MOVE 1 TO Wtrouve
        END-READ
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

       SUPPRIMER_PASS_EDITION.
         OPEN I-O fpass
         MOVE 0 TO Wfin
         MOVE 0 TO Wtrouve
         MOVE 0 TO Wtrouve
         OPEN INPUT fpass 
         MOVE fp_dateA TO dateA
         START fpass, KEY = fp_dateA
           INVALID KEY 
                DISPLAY "Il n'y a aucun pass d'ajouté " 
          "pour cet edition."
           NOT INVALID KEY
           MOVE 1 TO Wtrouve
             PERFORM WITH TEST AFTER UNTIL  Wfin = 1
                READ fpass NEXT RECORD
                AT END MOVE 1 TO Wfin
                NOT AT END
                 IF fp_dateA = dateA THEN
                   DELETE fpass END-DELETE
                 ELSE                      
                   MOVE 1 TO Wfin
                 END-IF
                END-READ
             END-PERFORM
           END-START
           CLOSE fpass
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
                ACCEPT Wprix
                MOVE fp_dateA TO fe_dateA
                PERFORM MAJ_PRIX_PASS_EDITION
                MOVE Wprix TO fp_prix
            END-EVALUATE
       IF choix = 1 OR choix = 2 THEN
         WRITE fpassTampon
         INVALID KEY
           DISPLAY "impossible de réécrire"
         NOT INVALID KEY
           DISPLAY "Pass modifié"
         END-WRITE
       ELSE
         REWRITE fpassTampon
         INVALID KEY
           DISPLAY "impossible de réécrire"
         NOT INVALID KEY
           DISPLAY "Pass modifié"
         END-REWRITE
       END-IF.

       MAJ_PRIX_PASS_EDITION.
         OPEN I-O feditions
         READ feditions
         INVALID KEY 
          DISPLAY 'La clé a été altérée'
         NOT INVALID KEY 
          MOVE fp_dateA TO dateA
          START fpass, KEY = fp_dateA
           INVALID KEY 
                DISPLAY "Il n'y a aucune réservation à mettre à jour " 
           NOT INVALID KEY
           MOVE 1 TO Wtrouve
             PERFORM WITH TEST AFTER UNTIL  Wfin = 1
                READ fpass NEXT RECORD
                AT END MOVE 1 TO Wfin
                NOT AT END
                 IF fp_dateA = dateA THEN
                   COMPUTE fe_resultat = fe_resultat - fp_prix + Wprix
                 ELSE                      
                   MOVE 1 TO Wfin
                 END-IF
                END-READ
             END-PERFORM
           END-START
        END-READ
        REWRITE fedTampon
        INVALID KEY
          DISPLAY 'Erreur lors de la mise à jour de l''édition'
        NOT INVALID KEY 
          DISPLAY 'Résultat modifié avec succès'
        END-REWRITE
        CLOSE feditions.



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
              DISPLAY ' |Afficher le nombre de groupe/edition : 5|'
              DISPLAY ' |Evolution deux années successives :    6|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_GROUPE
              WHEN 2 PERFORM AFFICHER_GROUPES
              WHEN 3 PERFORM SUPPRIMER_GROUPE
              WHEN 4 PERFORM MODIFIER_GROUPE
              WHEN 5 PERFORM NB_ARTISTE_EDITION
              WHEN 6 PERFORM EVO_ARTISTE_EDITION
              WHEN 7 PERFORM MOY_NB_ARTISTE
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
              MOVE 0 TO pos
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve=1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                NOT AT END
                IF fg_nom = nomGr THEN
                  MOVE 1 TO Wtrouve
                  DISPLAY 'trouvé'
                ELSE
                  COMPUTE pos = pos + 1
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
                ACCEPT nomGr
                MOVE fg_style to styleGr
              WHEN 2 
                DISPLAY 'Nouveau style = '
                ACCEPT styleGr
                MOVE fg_nom to nomGr
              END-EVALUATE
              END-PERFORM
              READ fgroupes
              PERFORM WITH TEST AFTER UNTIL pos=0
                READ fgroupes
                NOT AT END
                  COMPUTE pos = pos - 1
                END-READ
              END-PERFORM
              MOVE nomGr to fg_nom
              MOVE styleGr to fg_style
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
              OPEN I-O frepresentations
              MOVE 0 TO Wtrouve
            PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
              DISPLAY 'Année ?'
              ACCEPT fe_dateA
              OPEN INPUT feditions 
                PERFORM VERIF_EDITION
              CLOSE feditions
            END-PERFORM
            MOVE fe_dateA to frep_dateA
              PERFORM WITH TEST AFTER UNTIL frep_jour <= 3
                DISPLAY 'Jour de la représentation ?'
                ACCEPT frep_jour
              END-PERFORM
              PERFORM WITH TEST AFTER UNTIL frep_heureDebut >= 0000 AND frep_heureDebut < 2400
                DISPLAY 'Heure de début ? '
                ACCEPT frep_heureDebut
              END-PERFORM
              MOVE 0 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
                DISPLAY 'Nom du groupe ?'
                ACCEPT nomGr
              PERFORM VERIF_NOM_GROUPE
              END-PERFORM
              *> Incrémentation du nombre d'artiste
              OPEN I-O feditions
                 READ feditions
                 INVALID KEY
                   DISPLAY "Aucune édition à cette date."
                 NOT INVALID KEY
                  COMPUTE fe_nbArtiste = fe_nbArtiste + 1
                   REWRITE fedTampon
                 END-READ
               CLOSE feditions

              MOVE nomGr TO frep_nomSce
              MOVE 0 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
              DISPLAY 'Nom de la scène ?'
              ACCEPT frep_nomSce
              OPEN INPUT fscenes
               MOVE frep_nomSce to fs_nomSce
                MOVE frep_dateA to fs_dateA               
               READ fscenes
                INVALID KEY 
                 DISPLAY 'La scene n''existe pas '
                NOT INVALID KEY 
                 MOVE 1 TO Wtrouve
                 DISPLAY 'La scene est présente' 
               END-READ 
              CLOSE fscenes
              END-PERFORM
              PERFORM WITH TEST AFTER UNTIL frep_cachet GREATER 0
                DISPLAY 'Cachet ?'
                ACCEPT frep_cachet
              END-PERFORM
              OPEN INPUT fscenes

              Close fscenes
              PERFORM WITH TEST AFTER UNTIL frep_nbPersonneMax GREATER 0
                DISPLAY 'Nombre de personne max ?'
                ACCEPT frep_nbPersonneMax
              END-PERFORM
              WRITE frepTampon END-WRITE
          
              CLOSE frepresentations.

        AFFICHER_REPRESENTATION.
              OPEN INPUT frepresentations                        
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
                   DISPLAY '| Quitter                   :           0|'
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
                         REWRITE frepTampon 
                            INVALID KEY 
                            DISPLAY "*** ERREUR INTERNE (rewrite)"
                            NOT INVALID KEY
                            DISPLAY "ok."
                         END-REWRITE
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
             PERFORM WITH TEST AFTER UNTIL choix < 9
              DISPLAY " _______* Menu gestion des scènes *_______ "
              DISPLAY "|Ajouter scene            :              1|"
              DISPLAY "|Afficher scene / edition :              2|"
              DISPLAY "|Supprimer scene          :              3|"
              DISPLAY "|Mofifier scene           :              4|"
              DISPLAY "|_________________________________________|"

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
       MOVE 0 TO Wtrouve
       READ fscenes
        INVALID KEY 
         DISPLAY 'La scene n''existe pas '
        NOT INVALID KEY 
         MOVE 1 TO Wtrouve
         DISPLAY 'La scene est présente' 
       END-READ.


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
          DISPLAY "modification impossible scene non enregistrée"
        ELSE 
         PERFORM VERIF_PROGRAMME_SCENE    
         IF Wprog = 1
           DISPLAY 'Il y a une représentation programmée sur la scène'
         ELSE 
           OPEN I-O fscenes
           MOVE 1 TO choix 
           PERFORM WITH TEST AFTER UNTIL choix= 0
              DISPLAY " ________* Modification scènes *__________"
              DISPLAY "|Annuler              :                  0|"
              DISPLAY "|Modifier capicité    :                  1|"
              DISPLAY "|Modifier cout        :                  2|"
              DISPLAY "|_________________________________________|"

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


       *> Modification sur l'édition
       *> Sur le cout moyen d'une scene 
       *> Sur le resultat du festival 
       MODIFIER_SCENE_COUT.
       MOVE 0 TO fs_cout
       PERFORM WITH TEST AFTER UNTIL fs_cout > 0
         DISPLAY 'Saisir un cout supérieure à 0'
         ACCEPT fs_cout
       END-PERFORM.  


       *> FONCTION OK PLUS RIEN A TOUCHER SAUF OPTIMISATION OUVERTURE 
       *> DEMANDER A ANTOINE SO POSSIBILITE DE MODIFIER SA PROCEDURE 
       *> NOMME "VERIF EDITION"  

       *> Modification sur l'édition
       *> Sur le cout moyen d'une scene 
       *> Sur le resultat du festival 
       AJOUT_SCENES.
       MOVE 0 TO Wtrouve 
       DISPLAY "Saisir l'année"
       ACCEPT fe_dateA
       PERFORM VERIF_EDITION
      *> Si on a trouver l'edition 
       IF Wtrouve = 1
        MOVE 0 TO Wtrouve
        OPEN I-O fscenes
        DISPLAY "Saisir le nom de la scène"
        ACCEPT fs_nomSce
        MOVE fe_dateA TO fs_dateA
        PERFORM VERIF_SCENES
        *> Si la scene est inexistante  
        IF Wtrouve = 0 

         PERFORM MODIFIER_SCENE_COUT
         PERFORM MODIFIER_SCENE_CAPACITE
         *> Apres modification on ajoute la scene 

          WRITE fscTampon  
            INVALID KEY DISPLAY 'Scène non enregistré'
            *> Si la scene abin été renregistré 
            NOT INVALID KEY DISPLAY 'Scene enregistré'
              OPEN I-O feditions 
              *> On initialise les variables temporaires
              MOVE 0 TO WnbScene
              MOVE 0 TO WResTemp
              MOVE 0 TO WCouTemp

              *> On met a jour le nombre de  scene + le res de l'edition
              *> Et le cout moyen 
              MOVE fe_nbScene TO WnbScene
              MOVE fe_resultat TO WResTemp
              *> On calcul le cout total avant ajout 
              COMPUTE WCouTemp = fe_coutMoyenScene * WnbScene END-COMPUTE
              *> On ajoute le cout de la nouvelle scene  
              COMPUTE WCouTemp = WCouTemp + fs_cout END-COMPUTE
              *> On augmente le nombre de scene 
              COMPUTE WnbScene = WnbScene + 1 END-COMPUTE 
              *> On recalcul la moyenne 
              COMPUTE WCouTemp =  WCouTemp / WnbScene END-COMPUTE
              *> On met à jour le resultat du festival
              COMPUTE WResTemp = WResTemp - fs_cout END-COMPUTE 

              MOVE WResTemp TO fe_resultat
              MOVE WnbScene TO fe_nbScene
              MOVE WCouTemp TO fe_coutMoyenScene

              REWRITE fedTampon
              INVALID KEY DISPLAY 'Erreur lors de la mise à jour d''édition'
              NOT INVALID KEY DISPLAY 'Edition mise à jour'
              END-REWRITE
              CLOSE feditions
             
            END-WRITE 
            CLOSE fscenes
         PERFORM AFFICHER_SCENES
         PERFORM AFFICHER_EDITION
        ELSE 
          DISPLAY 'Ajout impossible, scène existante'
        END-IF
      *> Si edition non trouvé
       ELSE 
       
         DISPLAY "Edition inconnue, vérifier qu'une édition à été créée"
         "pour l'année spécifiée avant d'ajouter une scène"

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
           DISPLAY "  _______________* Menu *_________________"
           DISPLAY " |Afficher les éditions :                1|"
           DISPLAY " |Ajout d'une éditions :                 2|"
           DISPLAY " |Modifier la capacité d'une édition :   3|"
           DISPLAY " |Supprimer une édition                  4|"
           DISPLAY " |Afficher le résultat d'une édition     5|"
           DISPLAY " |Afficher le cout moyen d'une scène     6|"
           DISPLAY " |Afficher cout moyen des artistes       7|"
           DISPLAY " |Quitter :                              0|"
           DISPLAY " |________________________________________|"
           ACCEPT choixMenu
           EVALUATE choixMenu
             WHEN 1 PERFORM AFFICHER_EDITIONS
             WHEN 2 PERFORM AJOUT_EDITIONS
             WHEN 3 PERFORM MODIFIER_CAPACITE
             WHEN 4 PERFORM SUPPRIMER_EDITION
             WHEN 5 PERFORM AFFICHAGE_RESULTAT_EDITION
             WHEN 6 PERFORM AFFICHAGE_COUT_SCENES
             WHEN 7 PERFORM AFFICHAGE_COUT_ARTISTES
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
            PERFORM AFFICHER_EDITION
         END-READ
       END-PERFORM
       CLOSE feditions.

       AFFICHER_EDITION.
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
             DISPLAY "Cachet moyen : ",fe_coutArtistes.




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
           MOVE 0 TO fe_nbScene
           MOVE 0 TO fe_nbArtiste
           MOVE 0 TO fe_nbResaJourUn
           MOVE 0 TO fe_nbResaJourDeux
           MOVE 0 TO fe_nbResaJourTrois
           MOVE 0 TO fe_resultat
           MOVE 0 TO fe_coutMoyenScene
           MOVE 0 TO fe_coutArtistes
           MOVE fe_dateA TO fp_dateA
           PERFORM GENERER_PASS
           WRITE fedTampon
           MOVE 1 TO Wtrouve
        NOT INVALID KEY
           DISPLAY "Il y a déjà une édition enregistrée pour cette date."
         END-READ
       END-PERFORM
       CLOSE feditions.

       MODIFIER_CAPACITE.
       DISPLAY "*********"
       DISPLAY "Choisissez l'edition : "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
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

       SUPPRIMER_EDITION.
       DISPLAY "*********"
       DISPLAY "Choisissez l'edition parmi la liste: "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
         ACCEPT fe_dateA
         READ feditions
         INVALID KEY
           DISPLAY "Pas d'édition correspondante."
         NOT INVALID KEY
           PERFORM AFFICHER_EDITION
           MOVE fe_dateA TO fp_dateA
           PERFORM SUPPRIMER_PASS_EDITION
           DELETE feditions END-DELETE
           DISPLAY "Cette édition a été supprimée"
         END-READ
       CLOSE feditions.

       AFFICHAGE_ANNEES_EDITIONS.
       OPEN I-O feditions 
       MOVE 0 TO Wfin
       PERFORM WITH TEST AFTER UNTIL Wfin = 1
         READ feditions NEXT
           AT END 
             MOVE 1 TO Wfin
           NOT AT END
             DISPLAY "Année : ",fe_dateA
         END-READ
       END-PERFORM
       CLOSE feditions.
         
       AFFICHAGE_RESULTAT_EDITION.
       DISPLAY "*********"
       DISPLAY "Choisissez l'edition parmi la liste : "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
       ACCEPT fe_dateA
       READ feditions
         INVALID KEY
           DISPLAY "Pas d'éditions à cette date."
         NOT INVALID KEY
           DISPLAY "Reslutat : ",fe_resultat
       END-READ
       CLOSE feditions.

       AFFICHAGE_COUT_SCENES.
       DISPLAY "*********"
       DISPLAY "Choisissez l'edition parmi la liste : "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
       ACCEPT fe_dateA
       READ feditions
         INVALID KEY
           DISPLAY "Pas d'éditions à cette date."
         NOT INVALID KEY
           DISPLAY "Coût moyen d'une scène : ",fe_coutMoyenScene
       END-READ
       CLOSE feditions.
       *>Statistiques
       NB_ARTISTE_EDITION.
        DISPLAY 'Année ?'
        ACCEPT fe_dateA
        OPEN INPUT feditions 
       READ feditions
       INVALID KEY 
         DISPLAY 'pas d édition cette année'
       NOT INVALID KEY
         DISPLAY 'Nombre d artiste : ', fe_nbArtiste
       END-READ
       CLOSE feditions.

       EVO_ARTISTE_EDITION.
        DISPLAY 'Année de la deuxième édition : '
        ACCEPT fe_dateA
        OPEN INPUT feditions 
       READ feditions
       INVALID KEY 
         DISPLAY 'pas d édition cette année'
       NOT INVALID KEY
         DISPLAY 'Nombre d artiste N: ', fe_nbArtiste
         MOVE fe_nbArtiste TO nbArtisteN
        COMPUTE fe_dateA = fe_dateA - 1
         READ feditions
         INVALID KEY 
           DISPLAY "Aucune édition pour l''année spécifiée"
           MOVE 0 TO Wtrouve
         NOT INVALID KEY
           DISPLAY 'Nombre d artiste N-1: ', fe_nbArtiste
           COMPUTE nbArtisteN = nbArtisteN - fe_nbArtiste
           DISPLAY 'Evolution : ', nbArtisteN
         END-READ

       END-READ
       CLOSE feditions.

       MOY_NB_ARTISTE.
       OPEN INPUT feditions
       MOVE 0 TO Wfin
       MOVE 0 TO nbEdition
       MOVE 0 TO nbMoyArtiste
       PERFORM WITH TEST AFTER UNTIL Wfin=1
         READ feditions NEXT
           AT END
             MOVE 1 TO Wfin
           NOT AT END
            COMPUTE nbEdition = nbEdition + 1
            COMPUTE nbMoyArtiste = nbMoyArtiste + fe_nbArtiste
         END-READ
       END-PERFORM
       COMPUTE nbMoyArtiste = nbMoyArtiste / nbEdition
       DISPLAY "Nombre moyen d artiste : ", nbMoyArtiste
       CLOSE feditions.

       AFFICHAGE_COUT_ARTISTES.
       DISPLAY "*********"
       DISPLAY "Choisissez l'edition parmi la liste : "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
       ACCEPT fe_dateA
       READ feditions
         INVALID KEY
           DISPLAY "Pas d'éditions à cette date."
         NOT INVALID KEY
           DISPLAY "Coût moyen des artistes : ",fe_coutArtistes
       END-READ
       CLOSE feditions.

       
       