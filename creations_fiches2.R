#Connexion à la base de données Maskaret
library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)

#connexion à la bdd Moodle hébergée en local sur ma machine
con <- dbConnect(MariaDB(),
                 dbname="maskaret",
                 host="localhost",
                 user="root",
                 password="super"
)

# Requête et tri final
cours <- tbl(con, sql(
  "SELECT
      c.id AS course_id,
      c.fullname AS course_name,
      c.shortname AS course_shortname,
      c.summary AS description,
      cat1.name AS category,
      cat2.name AS subcategory,
      COUNT(DISTINCT ue.userid) AS nombre_inscrits
   FROM
      mdl_course c
   LEFT JOIN
      mdl_course_categories cat1 ON c.category = cat1.id
   LEFT JOIN
      mdl_course_categories cat2 ON cat1.parent = cat2.id
   LEFT JOIN
      mdl_enrol e ON c.id = e.courseid
   LEFT JOIN
      mdl_user_enrolments ue ON e.id = ue.enrolid
   WHERE
      c.category IS NOT NULL
   GROUP BY
      c.id, c.fullname, c.shortname, c.summary, cat1.name, cat2.name"
)) %>%
  collect() %>%
  arrange(course_shortname)  # Tri par code court du cours

# Affiche les premières lignes
head(cours)

# Ferme la connexion
dbDisconnect(con)




#Récupération de la table avec les cours
cours_data <- dbGetQuery(con,"SELECT * FROM cours")

#Récupération de la table avec les catégories
categorie_data <- dbGetQuery(con,"SELECT * FROM categorie") %>% 
  rename("categorie" = "nom","descriptif_categorie" = "descriptif")

#Récupération de la table avec les sous_catégories
souscategorie_data <- dbGetQuery(con,"SELECT * FROM souscategorie") %>% 
  rename("souscategorie" = "nom","descriptif_souscategorie" = "descriptif") %>% 
  left_join(categorie_data,by=c("categorie_id"="id"))

#Enrichissement de la table des cours avec les sous catégories et les catégories
cours_data <- cours_data %>% 
  left_join(souscategorie_data,by=c("sousCategorie_id"="id")) 

#Fermeture de la connexion à  la bdd après la récupération des données
DBI::dbDisconnect(con)


#Fonction pour nettoyer le nom
clean_filename <- function(name) {
  # Supprimer les accents
  name <- iconv(name, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Remplacer les caractères non alphanumériques par des underscores
  name <- gsub("[^[:alnum:]_]", "_", name)
  # Retirer les underscores en double ou en fin de nom
  name <- gsub("_+", "_", name)
  name <- gsub("_$", "", name)
  return(tolower(name)) # Convertir en minuscules pour uniformiser
}


# Boucle pour créer l'arborescence et générer les fiches----
for (i in 1:nrow(cours_data)) {
  row <- cours_data[i, ]
  
  # Créer le chemin pour la catégorie/sous-catégorie
  dir_path <- file.path("fiches_cours",clean_filename(row$categorie), 
                        clean_filename(row$souscategorie))
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  
  # Nom du fichier .qmd
  filename <- file.path(dir_path, 
                        paste0(clean_filename(row$nom), 
                               ".qmd"))
  
  # Contenu de la fiche de cours
  content <- glue::glue("

  ---
  title: \"{row$souscategorie} : {row$nom}\"
  ---


  - **Description** : {row$descriptif}
  - **Niveau** : {row$niveau}
  - **Durée** : {row$duree}
  ")
  
  # Écrire le contenu dans le fichier .qmd
  writeLines(content, filename)
}


#Création d'un Yaml qui reprend les rubriques des cours----

# Chemin pour le fichier `_quarto.yml`
quarto_yml_path <- file.path("_quarto.yml")

# Commence à écrire la structure de base pour le livre
cat("project:\n", file = quarto_yml_path)
cat("  type: book\n", file = quarto_yml_path, append = TRUE)
cat("book:\n", file = quarto_yml_path, append = TRUE)
cat("  title: \"Catalogue des e-formations sur Maskaret\"\n", file = quarto_yml_path, append = TRUE)
cat("  author: \"Equipe TICE\"\n", file = quarto_yml_path, append = TRUE)
cat("  cover-image: logo_cefil_new.jpg\n", file = quarto_yml_path, append = TRUE)
cat("  chapters:\n", file = quarto_yml_path, append = TRUE)
cat("    -index.qmd\n", file = quarto_yml_path, append = TRUE)

# Ecriture dans le YAML de toutes les références aux fiches 
#qmd dans l'arborescence
for (cat in unique(cours_data$categorie)) {
  cat(paste0("    - part: \"", cat, "\"\n"), file = quarto_yml_path, append = TRUE)
  cat("      chapters:\n", file = quarto_yml_path, append = TRUE)
  
  sous_cats <- unique(cours_data$souscategorie[cours_data$categorie == cat])
  
  for (sous_cat in sous_cats) {
    cours <- cours_data[cours_data$categorie == cat & cours_data$souscategorie == sous_cat, ]
    
    for (i in 1:nrow(cours)) {
      cours_file <- file.path("fiches_cours",clean_filename(cat), clean_filename(sous_cat), paste0(clean_filename(cours$nom[i]), ".qmd"))
      cat(paste0("        - file: ", cours_file, "\n"), file = quarto_yml_path, append = TRUE)
    }
  }
}

#Ecriture dans le YAML du dernier paragraphe avec les formats
cat("format:\n", file = quarto_yml_path,append = TRUE)
cat("  html:\n", file = quarto_yml_path, append = TRUE)
cat("    theme: cosmo\n", file = quarto_yml_path, append = TRUE)
cat("  pdf:\n", file = quarto_yml_path, append = TRUE)
cat("    documentclass: scrreprt\n", file = quarto_yml_path, append = TRUE)
cat("editor: visual\n", file = quarto_yml_path, append = TRUE)

