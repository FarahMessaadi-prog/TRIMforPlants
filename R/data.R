#' #' Données brutes d'exemple pour le pipeline TRIMforPlants
#'
#' Un jeu de données simulé de suivi de populations végétales, formaté
#' pour tester le pipeline de transformation et d'analyse des tendances (TRIM).
#' Ce jeu de données satisfait les critères de filtrage de base (précision, année, origine).
#'
#' @format Un data frame avec 12 lignes et 17 colonnes :
#' \describe{
#'   \item{NO_NOTE}{Identifiant unique de l'observation.}
#'   \item{NOM_COMPLET}{Nom du taxon observé.}
#'   \item{A_NOTE}{Année de l'observation.}
#'   \item{CO_CANTON}{Canton d'observation (GE, VD).}
#'   \item{INTRODUIT}{Statut d'origine (N: Naturel).}
#'   \item{CAT_ABONDANCE}{Code d'abondance (catégorie 1, 2, 3).}
#'   \item{ABONDANCE}{Valeur d'abondance numérique (NA pour cet exemple).}
#'   \item{XY_PRECISION}{Précision des coordonnées (50 mètres).}
#'   \item{OBSERVATEURS}{Observateur (simulé).}
#'   \item{UNITE_COMPTAGE}{Unité utilisée pour le comptage (I: Indivisu).}
#'   \item{...}{Autres colonnes requises par le mapping par défaut.}
#' }
#'
#' @usage data(trim_raw_data_example)
#'
#' @source Données simulées pour la vignette.
"trim_raw_data_example"



#'
#' Mapping des colonnes pour le format SIPV
#'
#' Cet objet contient la correspondance entre les noms de colonnes d’un jeu de données
#' brut (tel que fourni par le système SIPV) et ceux attendus par la fonction
#' \code{create_clean_data()}.
#'
#' Chaque élément de la liste associe un nom standardisé (utilisé dans le package)
#' au nom réel de la colonne dans le jeu de données brut.
#'
#' @format Une liste nommée avec les éléments suivants :
#' \describe{
#'   \item{obs_id}{Identifiant unique de la note d’observation (\code{NO_NOTE})}
#'   \item{taxon}{Nom scientifique complet de l’espèce (\code{NOM_COMPLET})}
#'   \item{observer}{Nom de l’observateur ou des observateurs (\code{OBSERVATEURS})}
#'   \item{day, month, year}{Date d’observation (\code{J_NOTE}, \code{M_NOTE}, \code{A_NOTE})}
#'   \item{canton, commune}{Localisation administrative (\code{CO_CANTON}, \code{NOM_COMMUNE})}
#'   \item{x, y}{Coordonnées (\code{X}, \code{Y})}
#'   \item{precision}{Précision de la localisation (\code{XY_PRECISION})}
#'   \item{xy_type}{Type de coordonnées (\code{XY_FORME})}
#'   \item{origine}{Origine ou statut d’introduction de l’espèce (\code{INTRODUIT})}
#'   \item{presence_absence}{Présence ou absence (\code{PRESENCE})}
#'   \item{unity}{Unité de comptage (\code{UNITE_COMPTAGE})}
#'   \item{abundance_code}{Catégorie d’abondance (\code{CAT_ABONDANCE})}
#'   \item{abundance_indication}{Valeur d’abondance observée (\code{ABONDANCE})}
#'   \item{effectif}{Effectif observé ou calculé (\code{EFFECTIF})}
#' }
#'
#' @source Données issues du système d’information SIPV (Service d’Information sur les Plantes Vasculaires)
"columns_map"


#' Table des intervalles d’abondance
#'
#' Cet objet fournit la correspondance entre les codes d’abondance et les intervalles numériques
#' utilisés pour estimer les effectifs d’observations.
#' Il peut être utilisé directement dans la fonction \code{create_clean_data()}.
#'
#' @format Un data frame avec les colonnes suivantes :
#' \describe{
#'   \item{code}{Code d’abondance (numérique)}
#'   \item{start}{Borne inférieure de l’intervalle d’abondance}
#'   \item{end}{Borne supérieure de l’intervalle d’abondance}
#' }
#'
#' @examples
#' data(abundance_interval_boundaries)
#' head(abundance_interval_boundaries)
#'
#' @source Grille d’abondance interne SIPV, adaptée du protocole InfoFlora.
"abundance_interval_boundaries"


#' Example synonym list template
#'
#' This object provides an example list structure for defining taxon synonyms.
#' Each element is a character vector where the first name is the canonical (accepted)
#' name and the following names are its synonyms.
#'
#' @format A list of character vectors.
#' @examples
#' synonymes <- synonymes_template
#' synonymes[[1]][1] <- "Aira caryophyllea L."  # canonical name
#' synonymes[[1]][2] <- "Aira caryophyllea subsp. caryophyllea"  # synonym
"synonymes_template"


