
# duplications occurr when students have submitted twice. Most often, this happens when people write their master's in pairs.
deduplicate_masters <- function(masters){
  masters |>
    #dplyr::mutate(!(sammendrag %in% sammendrag_alt)) |> #sjekk at det ikke bare er språkene som er byttet ## FUNGERER IKKE! FJERNER MASTERNE HELT!
    dplyr::mutate(abstract = refinr::key_collision_merge(abstract),
                  #abstract = refinr::n_gram_merge(abstract)
    ) |>
    dplyr::distinct(abstract, .keep_all = TRUE)
}
