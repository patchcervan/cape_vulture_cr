plotCondProp <- function(dat, var, group, breaks){

      cov_df <- dat %>%
            dplyr::select(group, step_id_, var) %>% 
            mutate(var_fct = cut(dat[[var]], breaks = breaks, labels = 1:breaks),
                   group_var = dat[[group]]) %>% 
            group_by(var_fct) %>% 
            summarize(prop = mean(group_var),
                      n = n())
      
      print(
            cov_df %>% 
                  ggplot() +
                  geom_point(aes(x = var_fct, y = prop)) +
                  xlab(var)
      )
       
      print(     
            cov_df %>% 
                  ggplot() +
                  geom_point(aes(x = var_fct, y = n)) +
                  xlab("Number of cases")
      )
      
}

