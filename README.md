### Import Data

    base_data = read_csv("qgdp_training.csv")

    ## Rows: 139 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): Date
    ## dbl (33): Agriculture, Forestry and Logging, Fishing, Aquaculture and Agricu...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    data = base_data %>% 
      select(Date, 
             `Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services`) %>% 
      mutate(Date = yearquarter(Date)) %>% 
      rename(FAAFFSS = `Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services`)
    data = as_tsibble(data, index=Date)
    rm(list="base_data") #Delete the original dataset

    data %>% autoplot() + 
      labs(title="New Zealand Fishing, Aquaculture and Agriculture, Forestry and Fishing Support Services (FAAFFSS) GDP ($ Millions) from 1987Q2 to 2021Q4", x= "Quarter", y="FAAFFSS")

    ## Plot variable not specified, automatically selected `.vars = FAAFFSS`

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png) \## Arima
Models

### Explore different ARIMA models

### Explain any transformations or differencing required

From data autoplot graph, we can see that

1.  the seasonal variations are increasing over time which suggests a
    multiplicative seasonality pattern. So we would do a log
    transformation to stabilize the variance. This transformation
    converts multiplicative relationships into additive ones, making the
    series easier to model.

2.  the data seems have seasonal effects that repeat every 4 quarters.
    So we would also differencing for seasonality to remove this effect.

3.there is an upward trend indicates that the mean of the series is not
constant over time. So we would need to do first difference to make the
series stationary in terms of the trend. However, as the data have a
strong seasonal pattern, we will apply seasonal differencing first,
because sometimes the resulting series will be stationary and there will
be no need for a further first difference.

    # Log transform to remove the non-constant variance. 

    data %>% 
      autoplot(log(FAAFFSS) ) + 
      labs(x = "x", y = "difference of y") + 
      theme_minimal() 

![](README_files/figure-markdown_strict/unnamed-chunk-2-1.png) so it
moved the large of change in variability. variance roughly constant.

    # differencing seasonality

    data %>% 
      autoplot(log(FAAFFSS) %>% difference(lag=4)) + 
      labs(x = "x", y = "difference of y") + 
      theme_minimal() 

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

after differencing seasonality, we can’t see any patterns now. we
removed the autocorrelation in seasonality. It also shows flat trend and
the mean is roughly constant, which suggests we removed the trend by
seasonal differencing and we don’t need to to first order difference
anymore. So we have stationary time series.

    # test if now the time series data are stationary and non-seasonaly with KPSS test
    data  %>%
      features(FAAFFSS , unitroot_kpss)

    ## Warning: 1 error encountered for feature 1
    ## [1] The `urca` package must be installed to use this functionality. It can be installed with install.packages("urca")

    ## # A tibble: 1 × 0

    data  %>%
      features(difference(log(FAAFFSS), 4) , unitroot_kpss)

    ## Warning: 1 error encountered for feature 1
    ## [1] The `urca` package must be installed to use this functionality. It can be installed with install.packages("urca")

    ## # A tibble: 1 × 0

so we can see the change of kpss after applying the transformation and
difference. there is now no evidence against the null hypothesis. which
means we get a stationary time series.

Or we can test how many difference we need to do to get a stationary
time series. It shows 1 is enough, which with aligns with what we did.

    data %>%
    features(FAAFFSS, unitroot_ndiffs)

    ## Warning: 1 error encountered for feature 1
    ## [1] The `urca` package must be installed to use this functionality. It can be installed with install.packages("urca")

    ## # A tibble: 1 × 0

    data %>%
    mutate(log_fa = log(FAAFFSS)) %>%
    features(log_fa, feat_stl)

    ## # A tibble: 1 × 9
    ##   trend_strength seasonal_strength_year seasonal_peak_year seasonal_trough_year
    ##            <dbl>                  <dbl>              <dbl>                <dbl>
    ## 1          0.992                  0.869                  2                    1
    ## # ℹ 5 more variables: spikiness <dbl>, linearity <dbl>, curvature <dbl>,
    ## #   stl_e_acf1 <dbl>, stl_e_acf10 <dbl>

    data %>%
    features(FAAFFSS, unitroot_nsdiffs)

    ## # A tibble: 1 × 1
    ##   nsdiffs
    ##     <int>
    ## 1       1

### Describe methodology used to create a shortlist of appropriate candidate ARIMA models (Fit both manually & automatically)

Now we will check on the acf and pacf to help us decide appropriate
candidate ARIMA models.

    data %>%gg_tsdisplay( log(FAAFFSS) %>% difference(4) , plot_type = "partial")

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

For the autoregression process, we check the pacf plot and see that the
significant peaks up to 4 and no significant peaks afterwards. it
suggest 4 autoregressive terms. likewise,for the moving average process,
we check the acf plot and see significant peaks up to the order of 3 and
no significant peaks afterwards. it suggest 3 moving average terms. Now
we add the manual fit and automatical fit together.

    # Manually and Automatically fit candidate models 
    fit <- data %>%
      model(arima_013 = ARIMA(FAAFFSS ~ pdq(0, 1, 3)),
            arima_413 = ARIMA(FAAFFSS ~ pdq(4, 1, 0)),
            stepwise = ARIMA(FAAFFSS),
            search = ARIMA(FAAFFSS, stepwise = FALSE))

    ## Warning: 1 error encountered for stepwise
    ## [1] The `urca` package must be installed to use this functionality. It can be installed with install.packages("urca")

    ## Warning: 1 error encountered for search
    ## [1] The `urca` package must be installed to use this functionality. It can be installed with install.packages("urca")

### Select the best model & explain why

We compare the result of both manual and auto fit ARIMA model and select
the model with smallest AICc as the best model. Here we will select the
manually fitted one “arima\_013” as our best model.

    glance(fit) %>% 
      arrange(AICc) %>% 
      select(.model:BIC)

    ## # A tibble: 2 × 6
    ##   .model    sigma2 log_lik   AIC  AICc   BIC
    ##   <chr>      <dbl>   <dbl> <dbl> <dbl> <dbl>
    ## 1 arima_013   355.   -582. 1175. 1175. 1189.
    ## 2 arima_413   356.   -582. 1176. 1177. 1194.

    # Report best model
    fit %>%
      select(arima_013) %>%
      report()

    ## Series: FAAFFSS 
    ## Model: ARIMA(0,1,3)(0,1,1)[4] 
    ## 
    ## Coefficients:
    ##           ma1      ma2     ma3     sma1
    ##       -0.1995  -0.2150  0.0925  -0.5801
    ## s.e.   0.0866   0.0938  0.1052   0.0746
    ## 
    ## sigma^2 estimated as 354.5:  log likelihood=-582.38
    ## AIC=1174.76   AICc=1175.23   BIC=1189.25

### Write the model equation in backshift notation

This specifies an ARIMA(0, 1, 3) model, which means: p=0: No
autoregressive (AR) terms. d=1: First difference to achieve stationary.
q=3: Three moving average (MA) terms. Backshift Notation:

*W*<sub>*t*</sub> = *Y*<sub>*t*</sub> − *Y*<sub>*t* − 1</sub>
The ARIMA(0, 1, 3) model is:

*W*<sub>*t*</sub> = *ϵ*<sub>*t*</sub> − *θ*<sub>1</sub>*ϵ*<sub>*t* − 1</sub> − *θ*<sub>2</sub>*ϵ*<sub>*t* − 2</sub> − *θ*<sub>3</sub>*ϵ*<sub>*t* − 3</sub>

Substituting *W*<sub>*t*</sub> gives:

*Y*<sub>*t*</sub> − *Y*<sub>*t* − 1</sub> = *ϵ*<sub>*t*</sub> − *θ*<sub>1</sub>*ϵ*<sub>*t* − 1</sub> − *θ*<sub>2</sub>*ϵ*<sub>*t* − 2</sub> − *θ*<sub>3</sub>*ϵ*<sub>*t* − 3</sub>

Rearranging the terms, we get:

*Y*<sub>*t*</sub> = *Y*<sub>*t* − 1</sub> + *ϵ*<sub>*t*</sub> − *θ*<sub>1</sub>*ϵ*<sub>*t* − 1</sub> − *θ*<sub>2</sub>*ϵ*<sub>*t* − 2</sub> − *θ*<sub>3</sub>*ϵ*<sub>*t* − 3</sub>

Where: - *Y*<sub>*t*</sub> is the value of the series at time *t*. -
*ϵ*<sub>*t*</sub> is the white noise error term at time *t*. -
*θ*<sub>1</sub>, *θ*<sub>2</sub>, *θ*<sub>3</sub> are the parameters of
the MA terms.

### Produce residual diagnostic plots (ggtsresiduals())

    # Check residuals
    fit %>% 
      select(arima_013) %>%
      gg_tsresiduals()

![](README_files/figure-markdown_strict/unnamed-chunk-12-1.png)

### Produce forecasts for h=8 quarters

    fit %>% 
      select(arima_013) %>%
      forecast(h = 8) %>%
      autoplot(data) + 
      theme_minimal()

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.png)

### Explain how prediction intervals are calculated for the method

Prediction intervals provide a range within which future observations
are expected to fall with a certain probability, typically 95%. Here’s
how prediction intervals are calculated for an ARIMA model:

1.  **Forecast Error Variance**: The prediction interval is based on the
    forecast error variance, which increases with the forecast horizon.
    For an ARIMA model, the forecast error at time *t* + *h* is
    calculated using the moving average (MA) process.

2.  **Standard Error of Forecast**: For an *h*-step ahead forecast, the
    standard error *σ*<sub>*h*</sub> can be computed using the residuals
    of the fitted model and the MA terms.

The standard error *σ*<sub>*h*</sub> accounts for the model parameters
and the white noise variance. It includes the accumulation of forecast
uncertainty as the horizon *h* increases.

1.  **Prediction Interval Formula**: For a 95% confidence interval, the
    critical value *z*<sub>0.025</sub> is 1.96. Using the forecasted
    values and the calculated standard error, construct the prediction
    intervals.

*Ŷ*<sub>*t* + *h*</sub> ± *z*<sub>*α*/2</sub> ⋅ *σ*<sub>*h*</sub>

Where: - *Ŷ*<sub>*t* + *h*</sub> is the forecasted value for time
*t* + *h*. - *z*<sub>*α*/2</sub> is the critical value from the standard
normal distribution for the desired confidence level (e.g., 1.96 for 95%
confidence). - *σ*<sub>*h*</sub> is the standard error of the *h*-step
ahead forecast.

## Neural Network Auto Regression Model

To understand how a Neural Network Auto Regression (NNAR) model works,
its important to first understand the workings of a standard feed
forward neural network.

### Understanding Neural Networks

A neural network is modelled after the way human brains work, in that
each thought is a result of multiple different neurons firing. Neural
networks act in a similar way, in that each prediction or output of the
model is a result of several nodes or “neurons” combining their
predictions together to come to a single prediction. In a feedforward
neural network, the network is comprised of an input layer which
includes the input data being fed into the model, optional hidden layers
which taken in the output from the previous layer as input, and an
output layer which returns the final prediction. The most basic neural
network has no hidden layers, which acts the same as a linear regression
model, since each input is given a weight, plus an additional bias
(intercept) parameter. Hidden layers take the outputs of their preceding
layers as inputs and then take a weighted linear sum of them, and alter
them by a non linear function such as sigmoid or relu. These hidden
layers allow non linearity to be captured by the model for more
complicated relationships.

<figure>
<img src="./media/nnet2-1.png" alt="Example Neural Network" />
<figcaption aria-hidden="true">Example Neural Network</figcaption>
</figure>

When training a feed forward neural network, each node in the network
begins with random weights, which are then adjusted to minimize some
metric - in the case of NNAR models this metric is MSE. Each iteration
of training updates the weights of each node in the network, and then
calculates the MSE of the models predictions when using these weights.
The network then uses a process called back propagation to assess each
node in the network to find which nodes were responsible for the most
error in the result, so that in the next iteration these nodes can have
their weights changed more drastically to improve the model as much as
possible.

### Understanding the Neural Network Auto Regression Model

NNAR models are neural networks which take lagged values of the time
series as inputs to the model in order to make predictions. These NNAR
models can have many hidden layers, but we fit ours with just one in
order to keep the model from being overly complicated. NNAR models are
described by the notation NNAR(p, P, k), where p represents the amount
of regular lagged inputs into the model, P represents the number of
seasonal lags included as input to the model, and k is the number of
neurons in the hidden layer. The easiest way to understand these
parameters is by looking at an example.

Say we are fitting an NNAR(3,2,3) model on a dataset with yearly
seasonality. Here we have a p parameter of 3, meaning in order to
predict *y*<sub>*n*</sub>, we will take the lagged inputs
*y*<sub>*n* − 1</sub>, *y*<sub>*n* − 2</sub>, *y*<sub>*n* − 3</sub>.
Additionally we have the P parameter of 2, meaning we will take the
seasonal lags *y*<sub>*n* − 12</sub>, *y*<sub>*n* − 24</sub>. This means
our model will have an input layer of 5 nodes which take in the lagged
inputs
*y*<sub>*n* − 1</sub>, *y*<sub>*n* − 2</sub>, *y*<sub>*n* − 3</sub>, *y*<sub>*n* − 12</sub>, *y*<sub>*n* − 24</sub>.
Also since our model has a k parameter of 3, it means there is a hidden
layer containing 3 neurons, which each take the outputs from the input
layer as their respective inputs.

For our model we use the NNETAR() function to fit an NNAR model for us
which will calculate optimal parameters for us. By default the model
will determine p by fitting a linear model on the seasonally adjusted
time series and taking the optimal number of lags according to AIC. The
P parameter will automatically be set to 1, and the k parameter is set
to (p + P + 1)/2.

### Model fitting

    NN.fit = data %>% 
      model(base_model = NNETAR()) %>% 
      report(base_model)

    ## Model not specified, defaulting to automatic modelling of the `FAAFFSS`
    ## variable. Override this using the model formula.

    ## Series: FAAFFSS 
    ## Model: NNAR(1,1,2)[4] 
    ## 
    ## Average of 20 networks, each of which is
    ## a 2-2-1 network with 9 weights
    ## options were - linear output units 
    ## 
    ## sigma^2 estimated as 492.1

    NN.fit %>% gg_tsresiduals()

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

    ## Warning: Removed 4 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    NN.fit %>% 
      forecast(h=8) %>% 
      autoplot(data) + 
      labs(title="FAAFFSS GDP ($ Millions) Predictions Using NNETAR Models", x= "Quarter", y="FAAFFSS")

![](README_files/figure-markdown_strict/unnamed-chunk-14-2.png)

### Prediction Interval Calculation

Since the NNAR model is not based on a well defined stochastic model,
there is not a straightforward method to calculate the prediction
interval. To account for this, bootstrapping is done in order to
determine the variability for each prediction.

The NNAR(1,1,2) model that we fit to the FAAFFSS data can be written as
*y*<sub>*t*</sub> = *f*(*z*<sub>*t* − 1</sub>) + *ϵ*<sub>*t*</sub>,
where *z*<sub>*t*</sub> = (*y*<sub>*t* − 1</sub>,*y*<sub>*t* − 4</sub>)
where the errors *ϵ*<sub>*t*</sub> are assumed to be normally
distributed around zero. Using this assumption, we can use our model to
make a prediction for the next time period *y*<sub>*t* + 1</sub> and add
a bootstrapped error, either drawn from the normal distribution or taken
from the sample of errors we saw in the training set. If we do this
several times (The forecast function does it 1000 times by default) then
we can approximate the prediction interval by finding the band which
contains 80% and 95% of (predictions + errors) at the next time period.
This process can be repeated for as many forecast periods as we like by
using the most recent prediction as the input for the new predictions.
