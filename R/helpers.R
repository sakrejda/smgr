
has_name = function(x, name) name %in% names(x)

count_unique = function(x) x %>% unique %>% length
max_nchar = function(x) x %>% nchar %>% max

short_form = function(x) {
  n_unique = count_unique(x)
  max_nchar = max_nchar(x)
  for (i in 1:max_nchar) {
    sub_x = substr(x, 1, i)
    if (count_unique(sub_x) == n_unique)
      break
  }
  return(sub_x)
}
  

