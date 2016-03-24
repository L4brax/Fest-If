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


       SELECT frepresentationsTemp ASSIGN TO "representations.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS frepTemp_stat
       RECORD KEY IS frep_cleRepT
       ALTERNATE RECORD KEY IS frep_dateAT WITH DUPLICATES
       ALTERNATE RECORD KEY IS frep_nomGrT WITH DUPLICATES
       ALTERNATE RECORD KEY IS frep_nomSceT WITH DUPLICATES.


       SELECT feditions ASSIGN TO "editions.dat"
       ORGANIZATION INDEXED
       ACCESS IS DYNAMIC
       FILE STATUS IS fe_stat
       RECORD KEY IS fe_dateA.

       SELECT fincrements ASSIGN TO "increment.dat"
       ORGANIZATION SEQUENTIAL 
       ACCESS IS SEQUENTIAL 
       FILE STATUS IS fi_stat.

       SELECT fusers ASSIGN TO "users.dat"
       ORGANIZATION IS INDEXED
       ACCESS IS dynamic
       FILE STATUS IS fu_stat
       RECORD KEY IS fu_id.
       
       

DATA DIVISION.
FILE SECTION.

  
         FD fusers.
         01 fiTampon.
          02 fu_id PIC A(30).
          02 fu_pass PIC A(30).
          02 fu_ad PIC 9(1). 

        FD fincrements. 
        01 finTampon.
          02 fi_idResa PIC 9(5).

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
          02 frep_cachet PIC S9(6).
          02 frep_nbPersonneMax PIC S9(30).

        FD frepresentationsTemp.
        01 frepTamponTemp.
          02 frep_cleRepT. 
            03 frep_jourT  PIC 9(2).
            03 frep_heureDebutT PIC 9(4).
            03 frep_dateAT PIC 9(4).
            03 frep_nomSceT PIC A(30).
          02 frep_nomGrT PIC A(30).
          02 frep_cachetT PIC S9(9).
          02 frep_nbPersonneMaxT PIC S9(30).

         FD feditions. 
         01 fedTampon. 
          02 fe_dateA PIC 9(4). 
          02 fe_capacite PIC 9(6).
          02 fe_NbScene PIC 9(2).
          02 fe_nbArtiste PIC 9(3).
          02 fe_nbResaJourUn PIC 9(4).
          02 fe_nbResaJourDeux PIC 9(4). 
          02 fe_nbResaJourTrois PIC 9(4). 
          02 fe_resultat PIC S9(30). 
          02 fe_coutScenes PIC 9(30). 
          02 fe_coutArtistes PIC 9(30).
          02 fe_nbRepresentations PIC 9(2).       
          02 fe_Ca PIC S9(30).                  


WORKING-STORAGE SECTION.
      *> Déclarations des zones de compte rendu  
        77 fs_stat PIC 9(2). 
        77 fp_stat PIC 9(2).
        77 fres_stat PIC 9(2).
        77 fg_stat PIC 9(2).
        77 fgTemp_stat PIC 9(2).
        77 frep_stat PIC 9(2).
        77 frepTemp_stat PIC 9(2).
        77 fe_stat PIC 9(2).
        77 fi_stat PIC 9(2). 
        77 Wcount PIC 9(3).
        77 Wallowed PIC 9(1).
        77 fu_stat PIC 9(2).


      *> Variables globales   
        77 choixMenu PIC 9(2).
        77 choix     PIC 9(2).
        77 Wfin      PIC 9.
        77 quitter PIC A.

      *> Variables utilisateur (NE PAS UTILISER DANS UN AUTRE CONTEXTE)  
        77 Wconfirm PIC A(30).
        77 WchoixCo PIC 9(1).
        77 Wconnect PIC 9(1). 



    	*>Variables pass réservation
        77 nomPa     PIC 9(3).
        77 dateA     PIC 9(4).
        77 dep       PIC 9(2).
        77 j         PIC 99.
        77 m         PIC 99.
        77 y         PIC 9999.
        77 Wtrouve   PIC 9(1).
        77 Wprix     PIC 9(4).
        77 Wcpt      PIC 9.
    	*>Variables groupe représentation
        77 nomGr PIC A(30).
        77 styleGr PIC A(30).
        77 pos PIC 9.
        77 posFin PIC 9.
        77 nomDernier PIC A(30).
        77 styleDernier PIC A(30).
        77 choixModifReserv PIC 9(2).

    	*>Variable scenes
      *>Variables editions
        77 WdateA PIC 9999.
        77 Wresultat PIC 9(9).
        77 WchoixModifReserv PIC 9(2).
        77 Wjour      PIC 9.
        77 minutes PIC 9(2).
        77 jourRep PIC S9(2).
        77 heureRep PIC S9(4).
        77 dispoGr PIC 9.

    	*>VARIABLES SCENE 
        77 WnbScene PIC 9(2).
        77 WCouTemp PIC 9(30).

      *> SUPPRIMER_SCENE
        77 WrepSc PIC 9(1).
      *>AJOUT_SCENES 
        77 WnomSc PIC A(4).
      *> MODIFIER_SCENE
        77 Wprog PIC 9(1).
      *> MOFIFIER CAPACITER SCENE
      *> Correspond au nombre de personne max 
      *> present sur un scene pour toute les representation  
        77 WnbMax PIC 9(2).   
      *> Annee
        77 Wyear PIC 9(4).
        77 conf PIC 9(1). 
        77 Wrep PIC 9(1).
      *>  Statistiques
        77 WcoutMoyenA PIC 9(30).
        77 WcoutMoyenS PIC 9(30).
        77 nbArtisteN PIC 9(3).
        77 nbMoyArtiste PIC 999v99.
        77 nbEdition PIC 9(3).

PROCEDURE DIVISION.

      *> Vérifiaction présence des fichiers 

       OPEN I-O fusers
       IF fu_stat =35 THEN
              OPEN OUTPUT fusers
              CLOSE fusers
              PERFORM INIT_ADMIN
       ELSE 
              CLOSE fusers
       END-IF


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

      *> Mise à jour des variables de connection en cas d'arrêt brutal 
       MOVE 0 TO WchoixCo 
       MOVE 0 TO Wconnect 


       PERFORM WITH TEST AFTER UNTIL choixMenu=0 
        PERFORM WITH TEST AFTER UNTIL choixMenu<9  
      *> METTRE ANNEE ACTUELLE POUR AFFICHER PROGR + PRIX
       

      *> Menu festivallier 
          DISPLAY "  ______________* Bienvenue *_____________" 
          DISPLAY ' |Quitter le programme        :          0|'        
          DISPLAY ' |Consulter programmation     :          1|'
          DISPLAY ' |Afficher les tarifs         :          2|'
          DISPLAY ' |Reserver                    :          3|'
          DISPLAY ' |Gestionnaire, se connecter  :          4|'
          DISPLAY ' |________________________________________|'
          DISPLAY 'Faites un choix : ' WITH NO ADVANCING
          ACCEPT choixMenu
           EVALUATE choixMenu
             WHEN 1 PERFORM AFFICHER_PROGRAMMATION 
             WHEN 2 PERFORM AFFICHER_PASS_EDITION
             WHEN 3 PERFORM AJOUTER_RESERVATION
             WHEN 4 PERFORM CONNEXION_USER
           END-EVALUATE
    

       IF Wconnect = 1 
      *> Menu gestionnaire 
       PERFORM WITH TEST AFTER UNTIL choixMenu=0 OR Wconnect = 0
         PERFORM WITH TEST AFTER UNTIL choixMenu<9  OR Wconnect = 0
              DISPLAY ' |   Connecté en tant que : ', fu_id   

              DISPLAY '  ___________* Menu gestionnaire *________'
              DISPLAY ' |Quitter le programme        :          0|'
              DISPLAY ' |Gestion des reservations    :          1|'
              DISPLAY ' |Gestion des pass            :          2|'
              DISPLAY ' |Gestion des groupes         :          3|'
              DISPLAY ' |Gestion des représentations :          4|'
              DISPLAY ' |Gestion des scènes          :          5|'
              DISPLAY ' |Gestion des éditions        :          6|'
              DISPLAY ' |Gestion des utilisateurs    :          7|'
              DISPLAY ' |Déconnexion                 :          8|'
              DISPLAY ' |Reset du jeu de données     :          9|'
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
               WHEN 7 PERFORM GESTION_USERS
               WHEN 8 PERFORM DECONNEXION_USER
               WHEN 9 PERFORM RESET_DONNEES
              END-EVALUATE
         END-PERFORM
       END-PERFORM
       END-IF

       END-PERFORM
       END-PERFORM 
  

       IF Wconnect =1 
       DISPLAY fu_id, 
       " Vous avez bien été déconnecté avant la fin du programme" 
       MOVE 0 TO Wconnect 
       CLOSE fusers
       END-IF

       STOP RUN.

       *> PARTIE UTILISATEUR 

       *> Gestion des utilisateur 
        GESTION_USERS. 
        PERFORM WITH TEST AFTER UNTIL WchoixCo=0 OR Wconnect = 0
          PERFORM WITH TEST AFTER UNTIL WchoixCo<9 OR Wconnect = 0   
            IF Wconnect = 1
            DISPLAY ' |   Connecté en tant que : ', fu_id  
            END-IF                               
            DISPLAY '  _________* Gestion des comptes *________'
            DISPLAY ' |Annuler                    :           0|'
            IF Wconnect = 0 
            DISPLAY ' |Connexion                  :           1|'
            END-IF
            IF Wconnect = 1 
            DISPLAY ' |Ajouter un utilisateur     :           2|'
            DISPLAY ' |Déconnexion                :           3|'
            DISPLAY ' |Changer mot de passe       :           4|'
            END-IF 
            DISPLAY ' |________________________________________|'
            DISPLAY 'Faites un choix : ' WITH NO ADVANCING
            ACCEPT WchoixCo
            EVALUATE WchoixCo
            WHEN 1 PERFORM CONNEXION_USER
            WHEN 2 PERFORM AJOUTER_USER
            WHEN 3 PERFORM DECONNEXION_USER 
            WHEN 4 PERFORM PASSWORD_USER
            END-EVALUATE
          END-PERFORM
        END-PERFORM.


        INIT_ADMIN. 
        OPEN I-O fusers

        MOVE "ADMIN" TO fu_id
        MOVE "ADMIN" TO fu_pass 
        MOVE 1 TO fu_ad

        WRITE fiTampon
        END-WRITE 

        CLOSE fusers.


        CONNEXION_USER. 
        IF Wconnect = 1 
         DISPLAY "Déconnection de l'utilisateur : " , fu_id
         PERFORM DECONNEXION_USER
        END-IF 

        OPEN I-O fusers 
        DISPLAY "Saisir votre nom d'utilisateur" 
        ACCEPT fu_id
        PERFORM VERIF_USER 
        IF Wtrouve = 1 
         DISPLAY "Saisir mot de passe"
         Accept Wconfirm 
         IF fu_pass = Wconfirm 
            MOVE 1 TO Wconnect
            DISPLAY "Connecté" 
         ELSE 
            DISPLAY "Echec de l'authentification"
            CLOSE fusers
         END-IF 
        ELSE 
         DISPLAY "Nom d'utilisateur inconnu"
         CLOSE fusers
        END-IF.

        DECONNEXION_USER. 
         MOVE 0 TO Wconnect
         MOVE 0 TO fu_ad
        CLOSE fusers.

        AJOUTER_USER. 
        IF Wconnect = 1 AND fu_ad =1
         PERFORM DECONNEXION_USER
         OPEN I-O fusers
         DISPLAY "Saisir votre nom d'utilisateur" 
         ACCEPT fu_id
         
         PERFORM VERIF_USER 
         IF Wtrouve = 1 
          DISPLAY "Nom utilisateur déjà utilidé, creation impossible" 
         ELSE 
            MOVE "A" TO Wconfirm 
            PERFORM TEST AFTER UNTIL Wconfirm = fu_pass 
             DISPLAY "Saisir mot de passe"
             ACCEPT fu_pass
             DISPLAY "Confirmation, veuillez saisir à nouveau votre mot de passe" 
             ACCEPT Wconfirm 
             IF Wconfirm = fu_pass 
              DISPLAY "Mots de passe identiques" 
             ELSE 
              DISPLAY "Mots de passe différents"
             END-IF 
            END-PERFORM 
            MOVE 0 TO fu_ad
            WRITE fiTampon
            INVALID KEY DISPLAY "Erreur lors de l'enregistrement"
            NOT INVALID KEY DISPLAY "Compte enregistré" 
            END-WRITE 

         END-IF
        ELSE 
         DISPLAY "Veuillez vous connecter en tant qu'administrateur avant de creer un compte"
        END-IF.

        PASSWORD_USER.
        IF Wconnect = 1
        DISPLAY 'Connecté en tant que : ', fu_id 
        READ fusers
        MOVE "A" TO Wconfirm 
        PERFORM TEST AFTER UNTIL Wconfirm = fu_pass 
         DISPLAY "Saisir mot de passe"
         ACCEPT fu_pass
         DISPLAY "Confirmation, veuillez saisir à nouveau votre mot de passe" 
         ACCEPT Wconfirm 
         IF Wconfirm = fu_pass 
          DISPLAY "Mots de passe identiques" 
         ELSE 
          DISPLAY "Mots de passe différents"
         END-IF 
        END-PERFORM
        REWRITE fiTampon
        INVALID KEY DISPLAY "Erreur lors de l'enregistrement"
        NOT INVALID KEY DISPLAY "Compte enregistré" 
        END-REWRITE
        ELSE 
        DISPLAY "Veuillez vous connecter avant de changer votre mot de passe"
        END-IF. 

        VERIF_USER. 
        MOVE 0 TO Wtrouve
        READ fusers 
        NOT INVALID KEY 
        MOVE 1 TO Wtrouve. 




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
              DISPLAY 'Désirez-vous afficher une programmation avant d''effectuer une réservation?'
              DISPLAY '1 pour oui, 0 pour non : 'WITH NO ADVANCING
              ACCEPT choix
              IF choix = 1 THEN 
                PERFORM WITH TEST AFTER UNTIL choix = 0
                  PERFORM AFFICHER_PROGRAMMATION
                  DISPLAY 'Désirez-vous afficher la programmation d''une autre édition?'
                  DISPLAY '1 pour oui, 0 pour non : '
                  WITH NO ADVANCING
                  ACCEPT choix
                END-PERFORM
              END-IF
              DISPLAY "____________* Nouvelle réservation *__________"
              DISPLAY 'Indiquer l''édition désirée : '
              WITH NO ADVANCING
              ACCEPT fres_dateA
              MOVE fres_dateA TO fp_dateA
              DISPLAY "_____________* Liste des pass *______________"
              PERFORM AFFICHER_PASS_EDITION
              IF Wtrouve = 1 THEN
                MOVE fres_dateA TO fe_dateA
                PERFORM WITH TEST AFTER UNTIL Wtrouve = 1 AND Wallowed = 1
                  DISPLAY 'Indiquer le numéro de pass : '
                  WITH NO ADVANCING
                  ACCEPT fres_nomPa
                  MOVE fres_nomPa TO fp_nomPa
                  PERFORM VERIF_PASS_ID
                  PERFORM VERIF_PASS_DISPO
                  IF Wallowed = 0 THEN
                    DISPLAY 'Il n''a plus de place pour l''un des jours'
                    'séléctionné'
                END-PERFORM

                DISPLAY 'Indiquer le prénom du participant : '
                WITH NO ADVANCING
                ACCEPT fres_prenom
                DISPLAY 'Indiquer son département de résidence : '
                WITH NO ADVANCING
                ACCEPT fres_dep
              
                PERFORM WITH TEST AFTER UNTIL j>00 AND j<=31 
                  DISPLAY 'Indiquer son jour de naissance : '
                  WITH NO ADVANCING
                  ACCEPT j
                END-PERFORM

                PERFORM WITH TEST AFTER UNTIL m>00 AND m<=12 
                  DISPLAY 'Indiquer son mois de naissance : '
                  WITH NO ADVANCING
                  ACCEPT m
                END-PERFORM

                PERFORM WITH TEST AFTER UNTIL y>1800 AND y<=2016 
                  DISPLAY 'Indiquer son année de naissance : '
                  WITH NO ADVANCING
                  ACCEPT y
                END-PERFORM
                STRING j m y INTO fres_dateNaissance

                PERFORM WITH TEST AFTER UNTIL Wcount > 0
                  MOVE 0 TO Wcount
                  DISPLAY 'Indiquer son adresse e-mail : '
                  WITH NO ADVANCING
                  ACCEPT fres_adresseEmail
                  INSPECT fres_adresseEmail TALLYING Wcount FOR CHARACTERS  AFTER INITIAL '@'
                END-PERFORM
                
                PERFORM WITH TEST AFTER UNTIL Wcount = 0
                  MOVE 0 TO Wcount
                  DISPLAY 'Indiquer son numéro de téléphone : '
                  WITH NO ADVANCING
                  ACCEPT fres_numTel
                  INSPECT fres_numTel TALLYING Wcount FOR ALL SPACES
                END-PERFORM
                MOVE fres_dateA TO fe_dateA 
                WRITE fresTampon 
                NOT INVALID KEY
                  PERFORM MAJ_NBRESERVATION
                  DISPLAY "___________________________________________"
                END-WRITE
              END-IF
              CLOSE freservations.

       AJOUTER_RESERVATION_ACTU.
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
       MOVE 1801 TO j.

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
                DISPLAY 'Indiquer le nouveau nom : '
                WITH NO ADVANCING
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
                  DISPLAY 'Indiquer le nouvel e-mail :' WITH NO ADVANCING
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
                  DISPLAY 'Indiquer le nouveau numéro de téléphone : '
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
                COMPUTE fe_Ca = fe_Ca + fp_prix
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
              DISPLAY 'Voulez-vous afficher les pass d''une édition avant de faire votre recherche?'
              DISPLAY '1 pour oui, 0 pour non :'
              WITH NO ADVANCING
              ACCEPT choix 
              IF choix = 1 THEN
                PERFORM RECHERCHE_PASS_EDITION
              END-IF
              DISPLAY 'Indiquer le numéro du pass : '
              WITH NO ADVANCING
              ACCEPT fres_nomPa 
              MOVE fres_nomPa TO nomPa
              START freservations, KEY = fres_nomPa
                INVALID KEY 
                     DISPLAY "Il n'y a aucune réservation à ce pass."
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
               DISPLAY 'Indiquer l''id du participant : '
               WITH NO ADVANCING
               ACCEPT fres_id 
               READ freservations
               INVALID KEY 
               DISPLAY "Il n'y a aucune réservation à cet id."
               NOT INVALID KEY
               MOVE 1 TO Wtrouve
               DISPLAY '  _____________* Réservation *_____________'
               PERFORM AFFICHER_RESERVATION
              CLOSE freservations.

       RECHERCHE_RESERVATION_EDITION.
              OPEN I-O freservations 
              MOVE 0 TO Wfin
              DISPLAY "Choisissez l'édition : "
              PERFORM AFFICHAGE_ANNEES_EDITIONS
              DISPLAY "Edition : " WITH NO ADVANCING
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
              DISPLAY 'Indiquer le département : '
              WITH NO ADVANCING
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
         DISPLAY 'Indiquer l''édition désirée : '
         WITH NO ADVANCING
         ACCEPT fp_dateA
       END-PERFORM
       DISPLAY 'Indiquer le nom du pass : '
       WITH NO ADVANCING
       ACCEPT fp_nomPa
       READ fpass
       INVALID KEY
         DISPLAY 'Indique le prix : '
         WITH NO ADVANCING
         ACCEPT fp_prix
         WRITE fpassTampon END-WRITE
       NOT INVALID KEY
         DISPLAY 'Impossible d''ajouter ce pass, il existe déjà.'
       CLOSE fpass.

       GENERER_PASS.
        OPEN I-O fpass 

        DISPLAY 'Indiquer le prix du pass premier jour : '
        WITH NO ADVANCING
        MOVE 1 TO fp_nomPa
        ACCEPT fp_prix
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass deuxième jour : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 2 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass troisième jour : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 3 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass premier et deuxième jour : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 12 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass deuxième et troisième jour : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 23 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass premier et troisième jour : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 13 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        DISPLAY 'Indiquer le prix du pass trois jours : '
        WITH NO ADVANCING
        ACCEPT fp_prix
        MOVE 123 TO fp_nomPa
        PERFORM VERIF_FORMAT_PRIX
        WRITE fpassTampon END-WRITE

        CLOSE fpass.

        VERIF_FORMAT_PRIX.
          PERFORM WITH TEST BEFORE UNTIL fp_prix > 0
          DISPLAY 'Veuillez saisir une valeur numérique : '
          WITH NO ADVANCING
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
       DISPLAY 'Indiquer l''édition : 'WITH NO ADVANCING
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
         	  MOVE 1 TO Wtrouve
        END-READ
        CLOSE fpass.

       RECHERCHE_PASS_ID.
        OPEN I-O fpass 
        DISPLAY 'Indiquer l''édition : 'WITH NO ADVANCING
        ACCEPT fp_dateA
        DISPLAY 'Indiquer le nom du pass : 'WITH NO ADVANCING
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
        DISPLAY 'Indiquer l''édition : 'WITH NO ADVANCING
        ACCEPT fp_dateA
        DISPLAY 'Indiquer le nom du pass : 'WITH NO ADVANCING
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
        PERFORM choix_MODIF_PASS
            EVALUATE choix
              WHEN 1 
                DELETE fpass
                DISPLAY 'Indiquer le nouveau nom : '
                WITH NO ADVANCING
                ACCEPT fp_nomPa
              WHEN 2 
                DELETE fpass
                DISPLAY 'Indiquer la nouvelle édition : '
                WITH NO ADVANCING
                ACCEPT fp_dateA
              WHEN 3 
                DISPLAY 'Indiquer le nouveau prix : '
                WITH NO ADVANCING
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
                   COMPUTE fe_Ca = fe_Ca - fp_prix + Wprix
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
              DISPLAY '  ____* Menu de gestion des groupes *_____'
              DISPLAY ' |Revenir au menu principal            : 0| '
              DISPLAY ' |Ajouter un groupe                    : 1|'
              DISPLAY ' |Afficher les groupes                 : 2|'
              DISPLAY ' |Supprimer un groupe                  : 3|'
              DISPLAY ' |Modifier un groupe                   : 4|'
              DISPLAY ' |Afficher le nombre de groupe/edition : 5|'
              DISPLAY ' |Evolution deux années successives    : 6|'
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
              MOVE 'n' to quitter
              DISPLAY "____________* Nouveau groupe *__________"
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 0 AND nomGr IS NOT = ' '
                    DISPLAY 'Indiquer le nom du groupe : '
                    WITH NO ADVANCING
                ACCEPT nomGr
                  IF nomGr = ' ' THEN
                  DISPLAY 'Le nom ne peut être vide !!'
                  ELSE
                  PERFORM VERIF_NOM_GROUPE
                END-IF
                IF Wtrouve = 1 THEN
                  DISPLAY 'le groupe existe dèjà !!'
                  DISPLAY 'voulez-vous quitter ? (n/o)'
                  ACCEPT quitter
                  IF quitter = 'o' THEN 
                    MOVE 0 TO Wtrouve
                  END-IF
                END-IF
              END-PERFORM
              IF quitter = 'n' THEN
              MOVE nomGr TO fg_nom
              PERFORM WITH TEST AFTER UNTIL fg_style IS NOT = ' '
              DISPLAY 'Indiquer le style du groupe : '
              WITH NO ADVANCING
              ACCEPT fg_style
              END-PERFORM
              OPEN EXTEND fgroupes
              WRITE fgTampon END-WRITE
              CLOSE fgroupes
              END-IF.
       
       VERIF_NOM_GROUPE.
              OPEN INPUT fgroupes
              MOVE 0 TO Wfin
              MOVE 0 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wfin = 1 OR Wtrouve = 1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                     
                NOT AT END
                IF fg_nom = nomGr THEN
                      MOVE 1 TO Wtrouve
                END-IF
              END-READ
              END-PERFORM
              CLOSE fgroupes.
       
        AFFICHER_GROUPES.
              OPEN INPUT fgroupes
              MOVE 0 TO Wfin
              DISPLAY '|____________________* Affichage des groupes  *_______________|'
              DISPLAY '|Groupe                        |Style                         |'
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
              READ fgroupes
                AT END MOVE 1 TO Wfin
                NOT AT END
                  DISPLAY '|',fg_nom,'|',fg_style,'|'
              END-READ
              END-PERFORM
              DISPLAY '|______________________________|______________________________|'
              CLOSE fgroupes.

       SUPPRIMER_GROUPE.
              OPEN INPUT fgroupes
              OPEN OUTPUT fgroupesTemp
              DISPLAY 'Indiquer le nom du groupe : '
              WITH NO ADVANCING
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
              DISPLAY 'Indiquer le nom du groupe : '
              WITH NO ADVANCING
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
                DISPLAY '  _______* Modification des groupes *_____'
                DISPLAY ' |Annuler             :                  0|'
                DISPLAY ' |Modifier le nom     :                  1|'
                DISPLAY ' |Modifier le style   :                  2|'
                DISPLAY ' |________________________________________|'
                DISPLAY 'Faites un choix : ' WITH NO ADVANCING  
              ACCEPT choixMenu
              EVALUATE choixMenu
              WHEN 1 
                DISPLAY 'Indiquer le nouveau nom : '
                WITH NO ADVANCING 
                ACCEPT nomGr
                MOVE fg_style to styleGr
              WHEN 2 
                DISPLAY 'Indiquer le nouveau style : '
                WITH NO ADVANCING 
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
                DISPLAY 'le groupe n''existe pas'
              END-IF.
      *>Gestion des représentations
       GESTION_REPRESENTATIONS.
              PERFORM WITH TEST AFTER UNTIL choix=0
         PERFORM WITH TEST AFTER UNTIL choix<5                 
              DISPLAY '  _* Menu de gestion des représentation *_'
              DISPLAY ' |Revenir au menu principal            : 0| '
              DISPLAY ' |Ajouter une nouvelle représentation  : 1|'
              DISPLAY ' |Afficher la programmation            : 2|'
              DISPLAY ' |Supprimer une représentation         : 3|'
              DISPLAY ' |Modifier  une représentation         : 4|'
              DISPLAY ' |________________________________________|'
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
              EVALUATE choix
              WHEN 1 PERFORM AJOUTER_NOUVELLE_REPRESENTATION
              WHEN 2 PERFORM AFFICHER_PROGRAMMATION
              WHEN 3 PERFORM SUPPRIMER_REPRESENTATION
              WHEN 4 PERFORM MODIFIER_REPRESENTATION
       END-EVALUATE
       END-PERFORM
       END-PERFORM.

       AJOUTER_NOUVELLE_REPRESENTATION.
        DISPLAY "____________* Nouvelle représentation *__________"
        OPEN I-O frepresentations
        MOVE 0 TO Wtrouve
        PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
        DISPLAY 'Indiquer l''édition de la représentation : '
        PERFORM AFFICHAGE_ANNEES_EDITIONS
        DISPLAY "Editions : " WITH NO ADVANCING
        ACCEPT fe_dateA
        OPEN I-O feditions 
          PERFORM VERIF_EDITION
        END-PERFORM
        MOVE 0 TO Wtrouve
          PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
          PERFORM AFFICHER_GROUPES
            DISPLAY 'Indiquer le nom du groupe : '
            WITH NO ADVANCING
            ACCEPT nomGr
          PERFORM VERIF_NOM_GROUPE
          END-PERFORM
          MOVE nomGr TO frep_nomGr
        MOVE fe_dateA to frep_dateA

        PERFORM WITH TEST AFTER UNTIL frep_jour <= 3 AND frep_jour > 0
          DISPLAY 'Indiquer le jour(1, 2, ou 3) : '
          WITH NO ADVANCING
          ACCEPT frep_jour
        END-PERFORM
        PERFORM WITH TEST AFTER UNTIL dispoGr = 0
        PERFORM WITH TEST AFTER UNTIL frep_heureDebut >= 0 AND frep_heureDebut < 24
          DISPLAY 'Indiquer l''heure de début (HH) : '
          WITH NO ADVANCING
          ACCEPT frep_heureDebut
        END-PERFORM
        PERFORM WITH TEST AFTER UNTIL minutes >= 0 AND minutes < 60
          DISPLAY 'et les minutes (MM) : '
          WITH NO ADVANCING
          ACCEPT minutes
        END-PERFORM
              COMPUTE frep_heureDebut = frep_heureDebut * 100 + minutes
              MOVE frep_heureDebut TO heureRep
              MOVE frep_jour TO jourRep
              MOVE frepTampon TO frepTamponTemp
              PERFORM VERIF_DISPO_GROUPE
              MOVE frepTamponTemp TO frepTampon
              END-PERFORM
         
              MOVE nomGr TO frep_nomSce
              MOVE 0 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
              MOVE frep_dateA to fs_dateA
              PERFORM AFFICHER_SCENES_ANNEE_SP
              DISPLAY 'Indiquer le nom de la scènes : '
              WITH NO ADVANCING
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
          DISPLAY 'Indiquer le cachet de l''artiste : '
          WITH NO ADVANCING
          ACCEPT frep_cachet
        END-PERFORM
        
              PERFORM WITH TEST AFTER UNTIL frep_nbPersonneMax <= fs_capacite
                DISPLAY 'Nombre de personne max : '
                WITH NO ADVANCING
                ACCEPT frep_nbPersonneMax
                IF frep_nbPersonneMax <= fs_capacite THEN
                  DISPLAY 'La capacité max de la scène est de : ',fs_capacite
                END-IF
              END-PERFORM
              WRITE frepTampon 
                INVALID KEY
                 DISPLAY 'erreur interne  ******'
               *> Si on a bien ajouté la représentation 
               *> on met à jour l'édition  
                NOT INVALID KEY
                DISPLAY 'représentation ajoutée'
                READ feditions
                  INVALID KEY DISPLAY "Erreur lors du chargement de l'édition"
                       
                  NOT INVALID KEY 
                    *> Incrémentation du nombre de représentation 
                    ADD 1 TO fe_nbRepresentations END-ADD 
                    ADD frep_cachet TO fe_coutArtistes END-ADD
                    REWRITE fedTampon 
                      INVALID KEY DISPLAY "Erreur de mise à jour de l'édition"
                      NOT INVALID KEY DISPLAY "Mise à jour de l'édition effectuée"
                    END-REWRITE
                  END-READ
                      *> Incrémentation du nombre d'artiste

                  START frepresentations,
                        KEY = frep_nomGr
                        INVALID KEY
                        READ feditions
                          NOT INVALID KEY
                            COMPUTE fe_nbArtiste = fe_nbArtiste + 1
                            REWRITE fedTampon 
                        END-READ
                    END-START
              END-WRITE
              CLOSE feditions
              CLOSE frepresentations.


        AFFICHER_REPRESENTATION.
          MOVE 0 to Wfin
          OPEN INPUT frepresentations                        
          DISPLAY 'Indiquer l''année de représentation : '
          WITH NO ADVANCING
          ACCEPT frep_dateA 
          START frepresentations,
          KEY = frep_dateA
            INVALID KEY
              DISPLAY 'Pas de représentation'
              MOVE 1 TO Wfin
            NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL Wfin = 1
            READ frepresentations NEXT RECORD
            AT END
              MOVE 1 TO Wfin
            NOT AT END
              DISPLAY 'Le groupe ',frep_nomGr,' joue sur ', frep_nomSce,'le ',frep_jour,' à ',frep_heureDebut
            END-READ
          END-PERFORM
           END-START
          CLOSE frepresentations.

        VERIF_DISPO_GROUPE.
          MOVE 0 TO dispoGr                       
          START frepresentations,
          KEY = frep_nomGr
            INVALID KEY
              DISPLAY 'Pas de représentation pour ce groupe'
              MOVE 1 TO Wfin
            NOT INVALID KEY
          PERFORM WITH TEST AFTER UNTIL Wfin = 1
            READ frepresentations NEXT RECORD
            AT END
              MOVE 1 TO Wfin
            NOT AT END
              IF frep_jour = jourRep THEN
                IF heureRep >= frep_heureDebut AND heureRep <= frep_heureDebut + 200
                DISPLAY 'Le groupe a déjà une représentation à ',frep_heureDebut
                MOVE 1 TO dispoGr
                END-IF
                IF frep_heureDebut >= heureRep AND frep_heureDebut <= heureRep + 200
                DISPLAY 'Le groupe a déjà une représentation à ',frep_heureDebut
                MOVE 1 TO dispoGr
                END-IF
              END-IF
            END-READ
          END-PERFORM
           END-START.

        AFFICHER_PROGRAMMATION.
          OPEN INPUT frepresentations                     
          MOVE 1 TO Wcount   
          PERFORM AFFICHAGE_ANNEES_EDITIONS
          DISPLAY 'Indiquer l''édition : '
          WITH NO ADVANCING
          ACCEPT frep_dateA 
          MOVE frep_dateA TO dateA
          PERFORM WITH TEST AFTER UNTIL Wcount > 3 OR Wtrouve = 0
            MOVE 0 TO Wcpt
            MOVE 0 TO Wfin
            MOVE dateA TO frep_dateA
            MOVE Wcount TO Wjour
            START frepresentations,
            KEY = frep_dateA
              INVALID KEY
                DISPLAY 'Pas d''édition cette année'
                MOVE 1 TO Wfin
                MOVE 0 TO Wtrouve
              NOT INVALID KEY
              MOVE 1 TO Wtrouve
              PERFORM WITH TEST AFTER UNTIL Wfin = 1
                READ frepresentations NEXT RECORD
                AT END
                  MOVE 1 TO Wfin
                NOT AT END
                  IF dateA = frep_dateA THEN
                    IF frep_jour = Wjour THEN
                    IF Wcpt = 0 THEN
                      DISPLAY '|______________________* Programmation jour ',Wcount,' *___________________|'
                      DISPLAY '|Groupe                        |Scène                         |Heure|'
                    END-IF
                      DISPLAY '|',frep_nomGr,'|', frep_nomSce,'|',frep_heureDebut,' |'
                      COMPUTE Wcpt = Wcpt + 1
                    END-IF
                    
                  ELSE
                    MOVE 1 TO Wfin
                  END-IF
                END-READ
              END-PERFORM
             END-START 
             COMPUTE Wcount = Wcount + 1
             IF Wtrouve = 1 THEN
             DISPLAY '|______________________________|______________________________|_____|'
             END-IF
          END-PERFORM
          CLOSE frepresentations.


       SUPPRIMER_REPRESENTATION.
              OPEN I-O frepresentations
              PERFORM AFFICHER_REPRESENTATION
                 MOVE fe_dateA TO frep_dateA
                 DISPLAY 'Nom de la scène :'
                 ACCEPT frep_nomSce
                 DISPLAY 'Jour :'
                 ACCEPT frep_jour
                 DISPLAY 'Indiquer l''heure de représentation (HH): '
                 WITH NO ADVANCING
                 ACCEPT frep_heureDebut
                 DISPLAY 'et les minutes (MM) : '
                 WITH NO ADVANCING
                 ACCEPT minutes
                 COMPUTE frep_heureDebut = frep_heureDebut * 100 + minutes
                 READ frepresentations
                  MOVE frep_nomGr TO nomGr
                 DELETE frepresentations RECORD
                  INVALID KEY
                    DISPLAY 'La représentation n existe pas'
                  NOT INVALID KEY
                    MOVE nomGr TO frep_nomGr
                   DISPLAY 'Représentation supprimée'
                   DISPLAY frep_nomGr
                    START frepresentations,
                    KEY = frep_nomGr
                      INVALID KEY
                        OPEN I-O feditions
                        READ feditions
                        NOT INVALID KEY
                         MOVE fe_coutArtistes TO WCouTemp
                         COMPUTE fe_nbArtiste = fe_nbArtiste - 1 END-COMPUTE 
                         COMPUTE fe_coutArtistes = WCouTemp - 1 END-COMPUTE 
                         REWRITE fedTampon
                      END-READ
                    CLOSE feditions
                     END-START
              CLOSE frepresentations.

       
       MODIFIER_REPRESENTATION.
          OPEN I-O frepresentations
          IF fres_stat =35 THEN
             DISPLAY 'Pas de représentation'
          ELSE    
             DISPLAY 'Indiquer le nom de la scène : '
             WITH NO ADVANCING
             ACCEPT frep_nomSce
             DISPLAY 'Indiquer l''édition : '
             WITH NO ADVANCING
             ACCEPT frep_dateA
             DISPLAY 'Indiquer le jour de représentation : '
             WITH NO ADVANCING
             ACCEPT frep_jour
             DISPLAY 'Indiquer l''heure de représentation : '
             WITH NO ADVANCING
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
                         DISPLAY 'Indiquer le jour (1, 2, 3) : '
                         WITH NO ADVANCING
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
                         DISPLAY 'Indiquer l''heure de début (HHMM) :'
                         WITH NO ADVANCING
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
                      DISPLAY 'Indiquer le nom du groupe : '
                      WITH NO ADVANCING
                            ACCEPT frep_nomGr
                     PERFORM VERIF_NOM_GROUPE
                     END-PERFORM
                   WHEN 4 
                      PERFORM WITH TEST AFTER UNTIL frep_cachet > 0
                      END-PERFORM
                    WHEN 5 
                      PERFORM WITH TEST AFTER UNTIL frep_nbPersonneMax > 0
                         DISPLAY 'Indiquer le nouveau cachet : '
                         WITH NO ADVANCING
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
              DISPLAY '|Quitter                  :              0|'
              DISPLAY "|Ajouter scene            :              1|"
              DISPLAY "|Afficher scene / edition :              2|"
              DISPLAY "|Supprimer scene          :              3|"
              DISPLAY "|Mofifier scene           :              4|"
              DISPLAY "|_________________________________________|"
              DISPLAY 'Faites un choix : ' WITH NO ADVANCING
              ACCEPT choix
       
              EVALUATE choix
               WHEN 1 PERFORM AJOUT_SCENES
               WHEN 2 OPEN INPUT fscenes PERFORM AFFICHER_SCENES_ANNEE CLOSE fscenes
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
       OPEN I-O fscenes
       PERFORM AFFICHER_SCENES_ANNEE
       DISPLAY "Saisir le nom de la scene"
       ACCEPT fs_nomSce
       PERFORM VERIF_SCENES
    
       IF Wtrouve = 0 
        DISPLAY 'La scene spécifié n''existe pas' 
       ELSE 
         PERFORM VERIF_PROGRAMME_SCENE
         
        IF Wprog = 1 
          DISPLAY "Impossible de supprimer la scene " 
          DISPLAY "Représentation programmé !"
        ELSE 
              OPEN I-O feditions
             *> Si la scene existe et n'a pas de représentation programmé 
              *> On initialise les variables temporaires
              MOVE fs_dateA TO fe_dateA
              MOVE 0 TO WnbScene
              MOVE 0 TO WCouTemp

              *> On met a jour le nombre de  scene + le res de l'edition
              *> Et le cout moyen 
              MOVE fe_nbScene TO WnbScene
              MOVE fe_coutScenes TO WCouTemp
              *> On enlève le cout de la scene  
              COMPUTE WCouTemp = WCouTemp - fs_cout END-COMPUTE
              *> On décrémente le nombre de scene 
              COMPUTE WnbScene = WnbScene - 1 END-COMPUTE 
     
              MOVE WnbScene TO fe_nbScene
              MOVE WCouTemp TO fe_coutScenes

              REWRITE fedTampon
              INVALID KEY DISPLAY 'Erreur lors de la mise à jour d''édition'
              NOT INVALID KEY DISPLAY 'Edition mise à jour'
              END-REWRITE

              DISPLAY "Etat de l'édition"
              PERFORM AFFICHER_EDITION

              CLOSE feditions

            DELETE fscenes RECORD
            INVALID KEY 
              DISPLAY "Impossible de supprimer "
            NOT INVALID KEY 
              DISPLAY "Supprimer"  
        END-IF
       END-IF
       CLOSE fscenes. 

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
       END-START    
       CLOSE frepresentations.


       MODIFIER_SCENE.
       OPEN I-O fscenes
       PERFORM AFFICHER_SCENES_ANNEE
        DISPLAY "Saisir le nom de la scene à modifier"
        ACCEPT fs_nomSce
        PERFORM VERIF_SCENES
        IF Wtrouve = 0 
          DISPLAY "modification impossible scene non enregistrée"
        ELSE 
           MOVE 1 TO choix 
           PERFORM WITH TEST AFTER UNTIL choix= 0
              DISPLAY " ________* Modification scènes *__________"
              DISPLAY '|Revenir au menu principal :             0| '
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
        END-IF 
       CLOSE fscenes.


       AFFICHER_SCENES.
       DISPLAY 'Nom:      : ' ,fs_nomSce
       DISPLAY 'Année     : ' ,fs_dateA
       DISPLAY 'capcicité : ' ,fs_capacite
       DISPLAY 'Cout      : ' ,fs_cout
       DISPLAY '_________________________________________'.


       *> Modification sur l'édition
       *> Sur le cout moyen d'une scene 
       *> Sur le resultat du festival 
       MODIFIER_SCENE_COUT.
        READ fscenes
        END-READ
        OPEN I-O feditions
             MOVE fs_dateA TO fe_dateA
             READ feditions
             *> On enlève la scene dans les statistiques 
             
              *> On initialise les variables temporaires
              MOVE 0 TO WCouTemp

              *> On met a jour le cout total scene 
              MOVE fe_coutScenes TO WCouTemp
              *> On enlève l'ancien cout de la  scene  
              COMPUTE WCouTemp = WCouTemp - fs_cout END-COMPUTE
        
              MOVE 0 TO fs_cout
              PERFORM WITH TEST AFTER UNTIL fs_cout > 0
              DISPLAY 'Saisir un cout supérieure à 0'
             ACCEPT fs_cout
              END-PERFORM

              MOVE fs_dateA TO fs_dateA
              READ feditions

              COMPUTE WCouTemp = WCouTemp + fs_cout END-COMPUTE 

              MOVE WCouTemp TO fe_coutScenes
     
              REWRITE fedTampon
              INVALID KEY DISPLAY 'Erreur lors de la mise à jour d''édition'
              NOT INVALID KEY DISPLAY 'Edition mise à jour'
              END-REWRITE

       CLOSE feditions.
       

       MODIFIER_SCENE_CAPACITE.
       *>  On initialisa notre variable max 
       MOVE 0 TO WnbMax
           OPEN INPUT frepresentations                        
                MOVE fs_dateA TO frep_dateA 
                START frepresentations,
                KEY = frep_dateA
                  INVALID KEY
                    DISPLAY 'Pas de représentation'
                    MOVE 1 TO Wfin
                  NOT INVALID KEY
                PERFORM WITH TEST AFTER UNTIL Wfin = 1
                  READ frepresentations NEXT RECORD
                  AT END
                    DISPLAY "C'est tout!"
                    MOVE 1 TO Wfin
                  NOT AT END
                    IF frep_nomSce = fs_nomSce AND WnbMax < frep_nbPersonneMax
                            MOVE frep_nbPersonneMax TO WnbMax
                    END-IF
                  END-READ
                END-PERFORM
                 END-START
                CLOSE frepresentations
                MOVE 0 TO fs_capacite
           PERFORM WITH TEST AFTER UNTIL fs_capacite > 0 AND fs_capacite<99
            DISPLAY 'Saisir une capacite supérieure à', WnbMax 'et inférieure à 99'
            DISPLAY WnbMax,' correpond au nombre maximum de personne présente '
            'pour les représentations programmées'
            ACCEPT fs_capacite
           END-PERFORM.    

          



       INIT_SCENE_CAPACITE.
       MOVE 0 TO fs_capacite
       PERFORM WITH TEST AFTER UNTIL fs_capacite > 0 AND fs_capacite<99
            DISPLAY 'Saisir une capacite supérieure à 0 et inférieure à 99'
            ACCEPT fs_capacite
       END-PERFORM.  

       *> Modification sur l'édition
       *> Sur le cout moyen d'une scene 
       *> Sur le resultat du festival 
       INIT_SCENE_COUT.
       MOVE 0 TO fs_cout
       PERFORM WITH TEST AFTER UNTIL fs_cout > 0
         DISPLAY 'Saisir un cout supérieure à 0'
         ACCEPT fs_cout
       END-PERFORM.  



       *> Modification sur l'édition
       *> Sur le cout moyen d'une scene 
       *> Sur le resultat du festival 
       AJOUT_SCENES.
       DISPLAY '____________* Ajout d''une scène *________'.
       MOVE 0 TO Wtrouve  
       DISPLAY "Indiquer l''édition : "
       PERFORM AFFICHAGE_ANNEES_EDITIONS
       OPEN I-O feditions
       DISPLAY "Edition : " WITH NO ADVANCING
       ACCEPT fe_dateA
       PERFORM VERIF_EDITION
      *> Si on a trouver l'edition 
       IF Wtrouve = 1
        MOVE 0 TO Wtrouve
        OPEN I-O fscenes
        DISPLAY "Indiquer le nom de la scène : "
        WITH NO ADVANCING
        ACCEPT fs_nomSce
        MOVE fe_dateA TO fs_dateA
        PERFORM VERIF_SCENES
        *> Si la scene est inexistante  
        IF Wtrouve = 0 

         PERFORM INIT_SCENE_CAPACITE
         PERFORM INIT_SCENE_COUT
         *> Apres modification on ajoute la scene 

          WRITE fscTampon  
            INVALID KEY DISPLAY 'Scène non enregistré'
            *> Si la scene abin été renregistré 
            NOT INVALID KEY DISPLAY 'Scene enregistré'
              *> On initialise les variables temporaires
              MOVE 0 TO WnbScene
              MOVE 0 TO WCouTemp
          

              *> On met a jour le nombre de  scene + le res de l'edition
              *> Et le cout moyen 
              MOVE fe_nbScene TO WnbScene
              MOVE fe_coutScenes TO WCouTemp
  
              *> On ajoute le cout de la nouvelle scene  
              COMPUTE WCouTemp = WCouTemp + fs_cout END-COMPUTE
              *> On augmente le nombre de scene 
              COMPUTE WnbScene = WnbScene + 1 END-COMPUTE 
     
              MOVE WCouTemp TO fe_coutScenes
              MOVE WnbScene TO fe_nbScene

              REWRITE fedTampon
              INVALID KEY DISPLAY 'Erreur lors de la mise à jour d''édition'
              NOT INVALID KEY DISPLAY 'Edition mise à jour'
              END-REWRITE
  
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

       END-IF
       CLOSE feditions.

         
       AFFICHER_SCENES_ANNEE.
       MOVE 0 TO WFin
       DISPLAY 'Indiquer l''édition : '

       PERFORM WITH TEST AFTER UNTIL fs_dateA > 1000
          DISPLAY 'Indiquer l''année (YYYY) : '
          WITH NO ADVANCING 
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
           NOT AT END   
              PERFORM AFFICHER_SCENES
          END-PERFORM.

       AFFICHER_SCENES_ANNEE_SP.
       MOVE 0 TO WFin
       OPEN INPUT fscenes
       START fscenes, KEY = fs_dateA
        INVALID KEY 
           DISPLAY 'Pas de scène pour l''année saisie' 
        NOT INVALID KEY
        DISPLAY 'Scènes de ',fs_dateA,' :'
              DISPLAY '|*____Affichage des scènes____*|'
              DISPLAY '|Nom                           |'
          PERFORM WITH TEST AFTER UNTIL WFin = 1
            READ fscenes NEXT 
            AT END 
              MOVE 1 TO WFin
           NOT AT END   
              DISPLAY '|', fs_nomSce, '|'
          END-PERFORM  
           DISPLAY '|______________________________|'
       CLOSE fscenes.
      *> Gestion editions

       GESTION_EDITIONS.
       PERFORM WITH TEST AFTER UNTIL choix=0
         PERFORM WITH TEST AFTER UNTIL choix<9                 
           DISPLAY "  _______________* Menu *_________________ "
           DISPLAY " |Afficher les éditions :                1|"
           DISPLAY " |Ajout d'une éditions :                 2|"
           DISPLAY " |Modifier la capacité d'une édition :   3|"
           DISPLAY " |Afficher le résultat d'une édition :   4|"
           DISPLAY " |Afficher le cout des scènes :          5|"
           DISPLAY " |Afficher cout des artistes :           6|"
           DISPLAY " |Quitter :                              0|"
           DISPLAY " |________________________________________|"
           DISPLAY 'Faites un choix : ' WITH NO ADVANCING
           ACCEPT choix
           EVALUATE choix
             WHEN 1 PERFORM AFFICHER_EDITIONS
             WHEN 2 PERFORM AJOUT_EDITIONS
             WHEN 3 PERFORM MODIFIER_CAPACITE
             WHEN 4 PERFORM AFFICHAGE_RESULTAT_EDITION
             WHEN 5 PERFORM AFFICHAGE_COUT_SCENES
             WHEN 6 PERFORM AFFICHAGE_COUT_ARTISTES
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
             DISPLAY "Benefice final : ",fe_Ca," euros"
             DISPLAY "Coût total des scènes  : ",fe_coutScenes
             DISPLAY "Coût total des artistes : ",fe_coutArtistes.




       AJOUT_EDITIONS.
       DISPLAY "Ajout d'une édition"
       DISPLAY "___________________________________________"
       OPEN I-O feditions
       MOVE 0 TO Wtrouve
       DISPLAY "Veuillez renseigner les informations suivantes :"
       PERFORM WITH TEST AFTER UNTIL Wtrouve = 1
         PERFORM WITH TEST AFTER UNTIL fe_dateA > 2015 AND fe_dateA < 2040
           DISPLAY "Indiquer l'annee de l'édition : " WITH NO ADVANCING
           ACCEPT fe_dateA
         END-PERFORM
         READ feditions
         INVALID KEY
           PERFORM WITH TEST AFTER UNTIL fe_capacite IS NUMERIC AND
       fe_capacite > 0 
             DISPLAY "Indiquer la capacité de l'édition : " WITH NO 
        ADVANCING
             ACCEPT fe_capacite
           END-PERFORM

           MOVE 0 TO fe_nbScene
           MOVE 0 TO fe_nbArtiste
           MOVE 0 TO fe_nbResaJourUn
           MOVE 0 TO fe_nbResaJourDeux
           MOVE 0 TO fe_nbResaJourTrois
           MOVE 0 TO fe_Ca
           MOVE 0 TO fe_coutScenes
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
         PERFORM WITH TEST AFTER UNTIL fe_dateA > 2015 AND fe_dateA < 2040
           DISPLAY "Edition : " WITH NO ADVANCING
           ACCEPT fe_dateA
         END-PERFORM
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
           DISPLAY "Chiffre d''affaires : ",fe_Ca
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
           DISPLAY "Coût moyen d'une scène : ", 
           DIVIDE fe_coutScenes BY fe_nbScene GIVING WcoutMoyenA END-DIVIDE
       END-READ
       CLOSE feditions.

      *>Statistiques
       NB_ARTISTE_EDITION.
        DISPLAY 'Année ?'
        PERFORM AFFICHAGE_ANNEES_EDITIONS
        DISPLAY "Edition : " WITH NO ADVANCING
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
           DISPLAY "Coût total des artistes : ",fe_coutArtistes
       END-READ
       CLOSE feditions.

       RESET_EDITION.
      *>Supprime les représentations
      *>Supprime les scènes
      *>Met à 0 le CA

       RESET_DONNEES.
       DISPLAY "*********"
       DISPLAY "Vous désirez créer un nouveau jeu de données."
       DISPLAY "Attention, cette action supprimera toutes les données de"
       DISPLAY "tous les fichiers et les remplacera par des nouvelles."
       DISPLAY "Pour info : ce nouveau jeu de données contient 3 éditions : 2015, 2016 et 2017."
       DISPLAY "Le jour 1 de l'édition 2016 est complet, soit 20 résas,"
       DISPLAY "Le jour 2 comprend 19 résas, soit une seule restante"
       DISPLAY "Le jour 3, seulement une résa."
       DISPLAY "Etes-vous sur ? :"
       DISPLAY " 1 - Oui"
       DISPLAY " 2 - Non"       
       MOVE 2 TO choix
       PERFORM WITH test AFTER UNTIL choix IS NUMERIC AND choix > 0 AND choix < 3
       DISPLAY "Faite un choix : "  WITH NO ADVANCING
       ACCEPT choix
           IF choix = 1 THEN
             OPEN OUTPUT fscenes
             OPEN OUTPUT fgroupes
             OPEN OUTPUT frepresentations
             OPEN OUTPUT feditions
             OPEN OUTPUT fpass
             OPEN OUTPUT freservations
             OPEN OUTPUT fincrements

             MOVE "ScèneA" TO fs_nomSce
             MOVE 2015 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneB" TO fs_nomSce
             MOVE 2015 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneC" TO fs_nomSce
             MOVE 2015 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneA" TO fs_nomSce
             MOVE 2016 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneB" TO fs_nomSce
             MOVE 2016 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneC" TO fs_nomSce
             MOVE 2016 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneA" TO fs_nomSce
             MOVE 2017 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneB" TO fs_nomSce
             MOVE 2017 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "ScèneC" TO fs_nomSce
             MOVE 2017 TO fs_dateA
             MOVE 6 TO fs_capacite
             MOVE 2000 TO fs_cout
             WRITE fscTampon END-WRITE

             MOVE "Meshuggah" TO fg_nom
             MOVE "Black Metal" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Nirvana" TO fg_nom
             MOVE "Grunge" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Foo Fighters" TO fg_nom
             MOVE "Rock américain" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Gorgoroth" TO fg_nom
             MOVE "Black Metal" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Casimir" TO fg_nom
             MOVE "DEATH metal" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Black Keys" TO fg_nom
             MOVE "Rock pop" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Noisebends" TO fg_nom
             MOVE "Rock" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Justin Bieber" TO fg_nom
             MOVE "Horrible" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Soviet Suprem" TO fg_nom
             MOVE "Electro Sovietique" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Zuchero" TO fg_nom
             MOVE "Truc italien" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Airbourne" TO fg_nom
             MOVE "Metal/Rock" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Francis Cabrel" TO fg_nom
             MOVE "Chanson française" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Lamb of God" TO fg_nom
             MOVE "Metal" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Queens of the stone age" TO fg_nom
             MOVE "Stoner" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Sublim text" TO fg_nom
             MOVE "Rock californien" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Jamiroquai" TO fg_nom
             MOVE "Funk" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Rolling Stones" TO fg_nom
             MOVE "Rock" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE "Lady gaga" TO fg_nom
             MOVE "Bizarre" TO fg_style
             WRITE fgTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1400 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Francis Cabrel" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1500 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Lady gaga" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1600 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Rolling Stones" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneC" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1700 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Jamiroquai" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1800 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Airbourne" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1900 TO frep_heureDebut
             MOVE 2015 TO frep_dateA
             MOVE "Lamb of god" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1400 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Justin Bieber" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneC" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1500 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Gorgoroth" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1600 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Foo Fighters" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1700 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Nirvana" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 26 TO frep_nbPersonneMax
             MOVE "ScèneC" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1800 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Casimir" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1900 TO frep_heureDebut
             MOVE 2016 TO frep_dateA
             MOVE "Black Keys" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 32 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1400 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Noisebends" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneC" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 1 TO frep_jour
             MOVE 1500 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Soviet Suprem" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 25 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1600 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Queens of the Stone Age" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 56 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2 TO frep_jour
             MOVE 1700 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Zuchero" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneC" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1800 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Meshuggah" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 30 TO frep_nbPersonneMax
             MOVE "ScèneA" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 3 TO frep_jour
             MOVE 1900 TO frep_heureDebut
             MOVE 2017 TO frep_dateA
             MOVE "Francis Cabrel" TO frep_nomGr
             MOVE 12000 TO frep_cachet
             MOVE 49 TO frep_nbPersonneMax
             MOVE "ScèneB" TO frep_nomSce
             WRITE frepTampon END-WRITE

             MOVE 2015 TO fe_dateA
             MOVE 30 TO fe_capacite
             MOVE 6 TO fe_nbArtiste
             MOVE 3 TO fe_NbScene
             MOVE 3 TO fe_nbResaJourUn
             MOVE 3 TO fe_nbResaJourDeux
             MOVE 3 TO fe_nbResaJourTrois
             MOVE 450 TO fe_resultat
             MOVE 12000 TO fe_coutScenes
             MOVE 72000 TO fe_coutArtistes
             MOVE 6 TO fe_nbRepresentations
             MOVE 570 TO fe_Ca
             WRITE fedTampon END-WRITE

             MOVE 2016 TO fe_dateA
             MOVE 20 TO fe_capacite
             MOVE 6 TO fe_nbArtiste
             MOVE 3 TO fe_NbScene
             MOVE 20 TO fe_nbResaJourUn
             MOVE 19 TO fe_nbResaJourDeux
             MOVE 1 TO fe_nbResaJourTrois
             MOVE 450 TO fe_resultat
             MOVE 2000 TO fe_coutScenes
             MOVE 72000 TO fe_coutArtistes
             MOVE 6 TO fe_nbRepresentations
             MOVE 12000 TO fe_Ca
             WRITE fedTampon END-WRITE

             MOVE 2017 TO fe_dateA
             MOVE 30 TO fe_capacite
             MOVE 6 TO fe_nbArtiste
             MOVE 3 TO fe_NbScene
             MOVE 0 TO fe_nbResaJourUn
             MOVE 0 TO fe_nbResaJourDeux
             MOVE 0 TO fe_nbResaJourTrois
             MOVE 450 TO fe_resultat
             MOVE 2000 TO fe_coutScenes
             MOVE 72000 TO fe_coutArtistes
             MOVE 6 TO fe_nbRepresentations
             MOVE 0 TO fe_Ca
             WRITE fedTampon END-WRITE

             MOVE 1 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 50 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 2 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 3 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 12 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 100 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 23 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 130 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 123 TO fp_nomPa
             MOVE 2015 TO fp_dateA
             MOVE 170 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 1 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 50 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 2 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 3 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 12 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 100 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 23 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 130 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 123 TO fp_nomPa
             MOVE 2016 TO fp_dateA
             MOVE 170 TO fp_prix
             WRITE fpassTampon END-WRITE

             MOVE 1 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 50 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 2 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 3 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 70 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 12 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 100 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 23 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 130 TO fp_prix
             WRITE fpassTampon END-WRITE
             
             MOVE 123 TO fp_nomPa
             MOVE 2017 TO fp_dateA
             MOVE 170 TO fp_prix
             WRITE fpassTampon END-WRITE
       
             MOVE 1 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE
             
             MOVE 2 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 3 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 4 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 5 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 6 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 7 TO fres_id
             MOVE 3 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 8 TO fres_id
             MOVE 3 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 9 TO fres_id
             MOVE 3 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2015 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE







             MOVE 1 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 2 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 3 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 4 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 5 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 6 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 7 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 8 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 9 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 10 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 11 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 12 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 13 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 14 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 15 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 16 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 17 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 18 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 19 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 20 TO fres_id
             MOVE 1 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 21 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 22 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 23 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 24 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 25 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 26 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 27 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 28 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 29 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 30 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 31 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 32 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 33 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 39 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 34 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 35 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 36 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 37 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 38 TO fres_id
             MOVE 2 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE

             MOVE 40 TO fres_id
             MOVE 3 TO fres_nomPa
             MOVE "Jean-Pascal-1" TO fres_prenom
             MOVE 14000 TO fres_dep
             MOVE 2016 TO fres_dateA
             MOVE "adresse@mail.com" TO fres_adresseEmail
             MOVE 0706050403 TO fres_numTel
             MOVE 01011990 TO fres_dateNaissance
             WRITE fresTampon END-WRITE             
              
             MOVE 45 TO fi_idResa
             WRITE finTampon END-WRITE

             CLOSE fincrements
             CLOSE freservations
             CLOSE fpass
             CLOSE feditions
             CLOSE frepresentations
             CLOSE fgroupes
             CLOSE fscenes
             DISPLAY "-------------------------------------------"
             DISPLAY "-- Les données ont été réinitialisées ! ---"
             DISPLAY "-------------------------------------------"
           END-IF
       END-PERFORM.
       















             



