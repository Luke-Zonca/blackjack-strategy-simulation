# Blackjack Strategy Simulation

An R Markdown project that simulates blackjack to compare two player strategies against a dealer and measure how table conditions affect win rates.

## What it does

- **Strategy 1:** hit until 17 or more
- **Strategy 2:** hit until 15 or more
- **Dealer:** hit until 17 or more
- Handles aces correctly (11, or 1 to avoid busting)
- Varies the number of players, number of decks, and reshuffle frequency, then plots win rates

## Run it

Requires R with the `rmarkdown` and `ggplot2` packages.

```r
install.packages(c("rmarkdown", "ggplot2"))
rmarkdown::render("Black_jack.Rmd")
```

The knitted write-up is in [`Black_jack.md`](Black_jack.md).

## Tech

R, ggplot2, R Markdown
