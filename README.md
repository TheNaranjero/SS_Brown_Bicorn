# Survey Sampling Simulator v0.4

An interactive R Shiny app, containerized with Docker and deployed on a Google Cloud VM, that builds a synthetic country and lets you see, for real, how sampling design, non-response, and calibration affect survey estimates.

The project covers the full path from statistical idea to a working product: the simulation and survey estimation (survey, data.table), the interactive interface (Shiny), packaging (Docker), and public deployment (Google Cloud), with the code on GitHub.

**Live app:** [LINK](http://34.58.27.185:3838/) | **Author:** Eduardo Andrés Villarruel | **Current version:** 0.4


![Mapa generado por la app mostrando varias provincias y ciudades](images/map%20tab.png)

![Menu de poblacion](images/pop%20tab.png)

![Menu de muestreo y resultado](images/sample%20tab.png)



## Why this project exists

When we run a real survey we can estimate the sampling error of our design, but we never find out whether the estimate was actually close to the truth, because the true population value is unknown.

In a simulation the full population is known. That makes it possible to draw a sample, estimate a quantity (here, total income), and compare it with the real value. I built this to see how different designs, non-response treatments, and calibration choices change the result.

## How it works

### 1. Generate a country

You choose:

- Number of households and average people per household
- Number of provinces
- Urbanization pattern: everyone in a mega-city, or spread across small towns
- Non-response intensity and type

The app then builds a population of provinces, cities, and households, and draws a map of it. The variables generated for each household are described in the [Variable dictionary](#variable-dictionary).

### 2. Design a survey

You choose:

- **Sample size**
- **Sampling strategy:**
  - Simple random sampling
  - Stratified by province, equal allocation
  - Stratified by province, proportional allocation
  - Stratified by city, equal allocation
  - Stratified by city, proportional allocation
- **Non-response treatment:**
  - Missing Completely at random
  - Missing Completely at random by province
- **Calibration variables:**
  - Gender
  - Age
  - Education
- **Calibration level** (the known population marginal totals used):
  - Province level
  - Country level

### 3. See the result

The app draws the sample, estimates total income, and reports how far the estimate is from the true population value.

## Tech stack

- **R**: `survey` (design-based estimation and calibration, following Thomas Lumley's *Complex Surveys*), `data.table` for fast data handling, `shiny` for the interface
- **Docker**: containerized app
- **Google Cloud**: deployment on a VM

## Run it locally

```bash
git clone [REPO URL]
cd [REPO NAME]
docker build -t sampling-sim .
docker run -p 3838:3838 sampling-sim
# then open http://localhost:3838
```

## Limitations

- The population is synthetic, so conclusions about real countries are only illustrative.
- A single run shows one random sample, so one result alone can mislead. Variation between samples is the point of the first roadmap item.
- Only one outcome (total income) is estimated.
- Not every sampling design and non-response mechanism is implemented yet (see roadmap).

## Roadmap

- [ ] **Repeated sampling:** run hundreds of replications and show the distribution of estimates, so bias and variance are visible for each design
- [ ] More designs: probability proportional to size, cluster, two-stage, systematic
- [ ] Non-response mechanism: MNAR (MCAR and MAR are already implemented)
- [ ] Missing data and imputation options
- [ ] Use a real population: Argentina's EPH, or census microdata from REDATAM, instead of a synthetic one

## What I learned

This was my first project with `data.table`, Shiny, Docker, and Google Cloud, built while working through Lumley's book. It turned theory about sampling error, non-response, and calibration into something I could experiment with.

## Variable dictionary

Each row of the population is a household. Variable names are in Spanish, as in the app.

| Variable | Description |
|---|---|
| `id_hogar` | Unique identifier of each household |
| `id_ciudad` | Unique identifier of each city |
| `id_provincia` | Unique identifier of each province |
| `var_hidden_1` ... `var_hidden_5` | Hidden variables, each a random number between 1 and 1000. Used to generate the variables below |
| `Ingreso` | Household income |
| `Edad_0_14` | Number of household members aged 0 to 14 |
| `Edad_15_64` | Number of household members aged 15 to 64 |
| `Edad_65_99` | Number of household members aged 65 to 99 |
| `Sexo_femenino` | Number of female members |
| `Sexo_masculino` | Number of male members |
| `Educacion_primario` | Number of members aged 15+ with primary education |
| `Educacion_secundario` | Number of members aged 15+ with secondary education |
| `Educacion_terciario` | Number of members aged 15+ with tertiary education |

## How the data is generated

Every variable follows the same recipe:

1. **Linear combination.** Draw random coefficients and combine the five hidden variables (plus province and city effects where noted).
2. **Standardize.** Subtract the mean and divide by the standard deviation of the combination, computed over the whole generated population (province and city effects included).
3. **Normal CDF.** Apply the standard normal CDF (`pnorm`) to turn the standardized value into a probability between 0 and 1. A sum of five uniform variables is approximately normal (central limit theorem), so these probabilities are approximately uniform.
4. **Target distribution.** Use that probability as the input to the quantile function of the distribution wanted (gamma, Poisson, Bernoulli, or binomial).

Because all variables are generated from the same five hidden variables, they are correlated with each other. This is what makes calibration informative.

### Income

1. Assign each hidden variable a random coefficient between -1.0 and +1.0, and add them together. Each province gets a random effect between -200 and +200, and each city between -100 and +100.

   ```
   a1 = b1*var_hidden_1 + ... + b5*var_hidden_5 + province_effect + city_effect
   ```

2. Standardize with the mean and standard deviation of `a1` over the population:

   ```
   a2 = (a1 - mean(a1)) / sd(a1)
   ```

3. Convert to a probability with the normal CDF:

   ```
   a3 = pnorm(a2)
   ```

4. Convert to a gamma distribution with mean 1000 and shape 4 (so rate = 4/1000 and standard deviation 500):

   ```
   Ingreso = qgamma(a3, shape = 4, rate = 4/1000)
   ```

### Age

New random coefficients are generated and the recipe is applied. The result `a3` is the probability that the household head is an adult (15 to 64) rather than elderly (65 to 99):

```
jefe_adulto = Bernoulli(p = a3)
```

Then, for each age group (0 to 14, 15 to 64, 65 to 99), new random coefficients are generated, the combination is standardized, and `pnorm` is applied. The resulting probability is passed to the Poisson quantile function, with mean `(average people per household - 1) / 3`. We subtract 1 because the household head is already assigned.

```
Edad_0_14  = qpois(a3, lambda = (avg_people_per_household - 1) / 3)
Edad_15_64 = qpois(a3, lambda = (avg_people_per_household - 1) / 3)
Edad_65_99 = qpois(a3, lambda = (avg_people_per_household - 1) / 3)
```

Each line uses a different `a3`, built from its own coefficients.

### Sex

The household size, `Edad_0_14 + Edad_15_64 + Edad_65_99`, is the number of trials `n` of a binomial distribution. The recipe is applied again, and `a3` is clipped between 0.25 and 0.75 to be used as the probability of the binomial. The binomial draw gives the number of female members, and the rest are male.

### Education

Only adults and elderly are considered, so `n = Edad_15_64 + Edad_65_99`.

- **Primary:** the recipe gives a proportion, and a binomial draw gives `Educacion_primario`.
- **Secondary:** same steps, but `n` is the remaining adults and elderly (the total minus those with primary education).
- **Tertiary:** everyone not assigned in the previous steps:

  ```
  Educacion_terciario = Total_15_99 - Educacion_primario - Educacion_secundario
  ```

## Changelog

### Version 0.1

The goal was to build the MVP, with these functions:

- Simulate a single hidden variable per unit (household)
- From it, compute a single response variable (income)
- Simulate a population of N households
- Draw a random sample of n households
- Estimate total income
- Upload to GitHub
- Deploy as an interactive web page

### Version 0.2

Added:

- Non-response (MCAR only)
- Option to correct for non-response (MCAR only)
- A simple map, purely decorative
- Sampling and results shown on the same tab
- RAM optimization:
  - Integers instead of doubles
  - Hidden variables discarded after use
  - Maximum households reduced from 10,000,000 to 1,000,000

  Together these reduced RAM use from 480 MB to 20 MB.

### Version 0.3

Added:

- Provinces
- Hidden variables representing the province effect
- People by age group
- Calibration
- Functions split into modules

### Version 0.4

- Hidden variables extended to 5
- Added province and city effects
- Average people per household and urban type (a single city vs. several towns) now affect the generated data
- Added sex and education (primary, secondary, tertiary)
- Non-response: added MCAR by province.
- Auxiliary variables: choose which to use (age, sex, education) and at which level (country, province, city)
- Improved maps