## Example using lfe and plm approach 

We will be using the _fatalities_ dataset from the `AER` package to estimate the relationship between drunk driving laws and traffic deaths. 

```{r}
data("Fatalities")
```

```{r}
fatdata <- Fatalities %>%
  mutate(fatal_rate = fatal / pop * 10000) 
```

Year-fixed effects by state: 
  
  ```{r}
ggplot(fatdata, aes(x = year, y = fatal_rate)) + 
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="line", aes(group=1))  + 
  stat_summary(fun.y=mean, geom="point") +
  facet_wrap(~state)
```


```{r}
fatdata88 <- Fatalities %>%
  mutate(fatal_rate = fatal / pop * 10000) %>% 
  dplyr::filter(year == "1988")
```

```{r}
plot(x = fatdata88$beertax, 
     y = fatdata88$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     pch = 20, 
     col = "steelblue")
```




```{r}
# OLS 
result_ols <- felm( fatal_rate ~ beertax  | 0 | 0 | 0, data = fatdata )
summary(result_ols, robust = TRUE)
```

```{r}
# State FE
result_stateFE <- felm( fatal_rate ~ beertax  | state | 0 | state, data = fatdata )
summary(result_stateFE, robust = TRUE)
```

This corresponds to the plm model formulas. Note the coefficients for $\beta_1$ are identical but the plm standard errors have not been "made" robust. 

```{r}
result_stateFE_plm <- plm(fatal_rate ~ beertax, index =c("state"), model = "within", data = fatdata)
summary(result_stateFE_plm)
```



```{r}
# State and Year FE
result_bothFE <- felm( fatal_rate ~ beertax  | state + year | 0 | state, data = fatdata )
summary(result_bothFE, robust = TRUE)
```

```{r}
result_stateFE_plm2 <- plm(fatal_rate ~ beertax, index =c("year", "state"), model = "within", effect = "twoways", data = fatdata) #  Note the twoways flag!
summary(result_stateFE_plm2)
```

```{r}
stargazer::stargazer(result_ols, result_stateFE, result_bothFE, type = "text")
```

What if we do not use the cluster-robust standard error?
  
  ```{r}
# State FE w.o. CRS
result_wo_CRS <- felm( fatal_rate ~ beertax  | state | 0 | 0, data = fatdata )

# State FE w. CRS
result_w_CRS <- felm( fatal_rate ~ beertax  | state | 0 | state, data = fatdata )
```


```{r}
# Report heteroskedasticity robust standard error and cluster-robust standard errors
stargazer::stargazer(result_wo_CRS, result_w_CRS,  type = "text", se = list(summary(result_wo_CRS)$rse, NULL))
```


If we just want to control for fix effect and only care about coefficients of interests, either felm and plm is a good choice. But if we want to know the effect of some specific groups, lm is preferred.
