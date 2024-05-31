---
author: Milica Stojaković, Marko Vukmirović, Stojan Gavrić, Jovana Ilin
date: "`r format(Sys.Date(), '%d. %B %Y.')`"
output: html_document
title: Analiza Ekoloških Parametara
---

# Uvod

Ovaj projekat predstavlja detaljnu analizu ekoloških parametara
prikupljenih sa različitih lokacija. Cilj analize je da se razumeju
ključni ekološki faktori koji utiču na biodiverzitet mahovina u
ekosistemima. Kroz ovaj projekat, biće primenjene metode deskriptivne
statistike, vizualizacije podataka, kao i napredne statističke analize
poput Generalizovanih Linearnih Modela (GLM) i Kanonske Korelacione
Analize (CCA).

## Ciljevi Projekta

-   **Deskriptivna Statistika**: Izvršiti osnovnu statističku analizu
    ekoloških parametara kao što su pH zemljišta, vlažnost, temperatura
    i druge.
-   **Vizualizacija Podataka**: Kreirati vizuelne prikaze koji će
    ilustrativno prikazati distribucije i odnose između različitih
    ekoloških faktora.
-   **Generalizovani Linearni Modeli**: Proceniti uticaj različitih
    prediktora na ekološke ishode koristeći GLM.
-   **Kanonska Korelaciona Analiza**: Istražiti odnose između dva seta
    varijabli kroz CCA.
-   **Analiza Zajednice**: Implementirati metode za detekciju struktura
    zajednice u ekološkim mrežama koristeći algoritme kao što je
    propagacija oznaka (Label Propagation).

## Struktura Dokumenta

Dokument je organizovan tako da pruži jasan i logičan pregled svih faza
projekta, od pripreme podataka do analize i zaključaka.

## Učitavanje Podataka

``` {r}
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("broom")) install.packages("broom")

# Učitavanje potrebnih biblioteka
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(igraph)
# Učitavanje Excel fajla
data <- read_excel("Env.xlsx")
# Prikaz strukture podataka
str(data)

# Prikaz prvih nekoliko redova podataka
head(data)
```

## **Deskriptivna statistika**

Deskriptivna statistika sažima i prikazuje skup podataka, pružajući uvid
u centralnu tendenciju i rasejanje. Ove statističke tehnike sažimaju i
opisuju podatke, pružajući korisne uvide. Pored minimalnih i maksimalnih
vrednosti, u obzir smo uzimali i:

-   **Srednju vrednost (Prosek)**: Aritmetička sredina skupa vrednosti.
    Predstavlja centralnu vrednost oko koje se podaci grupišu.

-   **Standardnu devijaciju**: Mera rasprostranjenosti podataka oko
    srednje vrednosti.

``` {r}
summary(data)

numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

summary_stats <- apply(numeric_data, 2, function(x) c(min(x), max(x), mean(x), sd(x)))
summary_stats



numeric_data <- data %>% select(where(is.numeric))


summary_stats <- numeric_data %>%
  summarise_all(list(
    mean = ~mean(. , na.rm = TRUE),
    max = ~max(. , na.rm = TRUE),
    min = ~min(. , na.rm = TRUE),
    sd = ~sd(. , na.rm = TRUE)
  )) %>%
  pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")


summary_stats_long <- summary_stats %>%
  pivot_longer(cols = -variable, names_to = "statistic", values_to = "value")


ggplot(summary_stats, aes(x = variable)) +
  geom_errorbar(aes(ymin = min, ymax = max, color = "Range (Min to Max)"), width = 0.2) +
  geom_point(aes(y = mean, color = "Mean"), size = 3) +
  geom_point(aes(y = mean + sd, color = "Mean + SD"), shape = 4, size = 3) +
  geom_point(aes(y = mean - sd, color = "Mean - SD"), shape = 4, size = 3) +
  scale_color_manual(
    name = "Statistike",
    values = c("Range (Min to Max)" = "black", "Mean" = "blue", "Mean + SD" = "red", "Mean - SD" = "red")
  ) +
  labs(
    title = "Grafikoni za deskriptivnu statistiku za svaki parametar",
    x = "Varijabla",
    y = "Vrednost"
  ) +
  theme_minimal()
```

-   Vizualizaciju desktriptivne statistike smo prikazali putem Box
    plotova. Box plotovi prikazuju medijanu, kvartile i ekstremne
    vrednosti.

``` {r}
data$category <- ifelse(substr(data[[1]], 1, 2) == "ST", "Strazilovo", 
                        ifelse(substr(data[[1]], 1, 1) == "D", "Dumbovo",
                            ifelse(substr(data[[1]], 1, 1) == "V", "Vrdnik",
                                ifelse(substr(data[[1]], 1, 2) == "LV", "Lazin Vir",
                                    ifelse(substr(data[[1]], 1, 1) == "J", "Jazak",
                                        ifelse(substr(data[[1]], 1, 2) == "KB", "Kanov Breg", "Papratski Do")
                                    )
                                )
                            )
                        )
)

data_numeric <- data %>% select_if(is.numeric) %>% cbind(category = data$category)



data_long <- data_numeric %>% pivot_longer(cols = -category, names_to = "variable", values_to = "value")


ggplot(data_long, aes(x = category, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.spacing = unit(2, "lines")) +
  labs(title = "Box plot svaku lokaciju", x = "Lokacija", y = "Vrednost")
```

# GLM - Generalizovani linearni model

**Generalizovani linearni model (GLM)** je opšti okvir u statistici koji
se koristi za modelovanje odnosa između zavisne promenljive (ishoda) i
jedne ili više nezavisnih promenljivih (prediktora). GLM proširuje
klasični linearni model (LM) tako da može da se bavi situacijama gde su
pretpostavke o normalnoj raspodeli greške i linearnosti odnosa
neadekvatne. GLM se sastoji od tri glavna dela:

**1. Linearni prediktor:** Kombinacija nezavisnih promenljivih i
njihovih koeficijenata. U matematičkom obliku, to je: η=Xβ gde je η
linearni prediktor, X je matrica nezavisnih promenljivih, a β je vektor
koeficijenata.

**2. Funkcija veze (link funkcija):** Funkcija koja povezuje očekivanu
vrednost zavisne promenljive sa linearnim prediktorom. Neke često
korišćene link funkcije uključuju:

-   Identitetska funkcija (g(μ)=μ): Koristi se kada je odnos između
    zavisne i nezavisnih promenljivih linearni, kao u standardnoj
    linearnoj regresiji.
-   Logit funkcija (g(μ)=log(μ/(1−μ))): Koristi se u logističkoj
    regresiji za binarne ishode.
-   Log funkcija (g(μ)=log(μ)): Koristi se u Poisson-ovoj regresiji za
    brojanja i modele sa pozitivnim vrednostima.
-   Probit funkcija: Koristi se za binarne podatke kada se pretpostavlja
    da je latentna varijabla normalno distribuirana.

**3. Porodična funkcija raspodele:** Vrsta raspodele koja opisuje
zavisnu promenljivu. GLM može koristiti različite porodice raspodela,
kao što su:

-   Normalna raspodela (Koristi se za kontinuirane zavisne promenljive
    sa konstantnom varijancom.).

-   Binomna raspodela (za binarne podatke).

-   Poisson-ova raspodela (za brojanja ili retke događaje).

-   Gamma raspodela (za pozitivne kontinuirane podatke, naročito kada je
    varijanca proporcionalna kvadratu srednje vrednosti).

U većini statističkih softvera i programskih jezika kao što su R, Python
i SAS, postoje ugrađene funkcije za fitovanje GLM-a. Na primer, u R-u se
koristi funkcija $glm()$ iz osnovne statističke biblioteke čiji izlaz
uključuje procene koeficijenata modela, standardnih grešaka, p-vrednosti
i statistike dobrog uklapanja.

GLM model je korišćen nad podacima iz tabele za utvrđivanje uticaja
svake od datih promenljivih na Shannon-ov indeks diverziteta flore
mahovina na lokalitetima koji su prethodno navedeni.

Iz rezultata koje smo dobili možemo zaključiti da na indeks diverziteta
utiče vlažnost zemljišta, pH površinskog sloja zemljišta i pokrovnost
zeljastih vaskularnih biljaka. Shannon-ov indeks diverziteta se povećava
sa povećanjem pH vrednosti zemljišta, sa povećanjem vlažnosti zemljišta
i žbunastih biljaka.

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = tn)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "Broj drvenastih biljaka") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = pH)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "pH vrednost zemljišta") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = sm)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "vlažnost zemljišta") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = st)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "temperatura zemljišta") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = hc)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "pokrovnost zeljastih biljaka") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = lc)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "pokrovnost stelje") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = sd)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "udaljenost od potoka") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = bn)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "broj žbunastih biljaka") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

``` {r}
data <- read_excel("EnvV.xlsx")

ggplot(data, aes(x = Shannon, y = ft)) + 
  geom_point(color = "red", size = 3, alpha = 0.7) + 
  geom_smooth(method = "glm", method.args = list(family = gaussian), color = "blue", linetype = "solid", size = 1.5) +  
  labs(x = "Shannon", y = "tip šume") + 
  ggtitle("Generalized Linear Model") +
  theme_classic(base_size = 15) + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),  
    axis.title.x = element_text(face = "italic", size = 14),  
    axis.title.y = element_text(face = "italic", size = 14),  
    axis.text = element_text(size = 12),  
    axis.line = element_line(color = "grey15", size = 1),  
    panel.grid.major = element_line(color = "gray78", size = 0.5),  
    panel.grid.minor = element_blank()  
  )
```

# Kanonska Korelaciona Analiza

Kanonska korelaciona analiza (CCA) je statistička metoda koja se koristi
za istraživanje veza između dva skupa promenljivih. Cilj CCA je da
identifikuje i kvantifikuje parove kanonskih varijabli (linearnih
kombinacija originalnih promenljivih iz oba skupa) koje imaju najveći
mogući korelacioni koeficijent.

``` {r}
# Učitavamo paket za rad sa Excel fajlovima
library(readxl)

# Učitavamo podatke iz Excel fajlova
env <- read_excel("Env.xlsx")
spec <- read_excel("Spec.xlsx")

# Menjamo ime kolone 'A' u 'site'
colnames(env)[1] <- "site"

head(env)
head(spec)
```

``` {r}
site_labels <- env$site  # Čuvamo nazive lokacija u posebnoj varijabli kako bismo mogli lako da im pristupimo kasnije
env_numeric <- env[, -1]  # Kreiramo novi data frame-a bez kolone 'site' koji ćemo koristiti u analizi ---> varijable koje ulaze u CCA moraju biti numeričke
```

``` {r}
# Učitavamo vegan paket
library(vegan)

# Izvršavanje CCA
cca_model <- cca(spec ~ ., data = env_numeric)

# Prikaz rezultata
summary(cca_model)
```

### Grafik koji pokazuje vrednosti sopstvenih vrednosti menjaju sa svakom osom što pomaže u vizuelizaciji značaja svake ose u analizi.

``` {r}
library(ggplot2)
eigenvalues <- c(0.9092, 0.4577, 0.30395, 0.26126, 0.22249, 0.10044, 0.08125, 0.05702, 0.026068)
eigen_df <- data.frame(Axis = 1:9, Eigenvalue = eigenvalues)
ggplot(eigen_df, aes(x = Axis, y = Eigenvalue)) +
  geom_line() + geom_point() + scale_x_continuous(breaks = 1:9) +
  labs(title = "Grafik sopstvenih vrednosti", x = "Kanoničke ose", y = "Eigenvalue")
```

### Grafik koji pokazuje procenat varijanse objašnjene svakom kanonskom osom

``` {r}
# Originalne proporcije
proportions <- c(0.2357, 0.1187, 0.07881, 0.06774, 0.05769, 0.02604, 0.02107, 0.01478, 0.006759)

# Proširivanje proporcija za ose koje nedostaju
full_proportions <- c(proportions, rep(0, 9 - length(proportions)))

# Kreiranje dataframe-a
prop_df <- data.frame(Axis = 1:9, Proportion = full_proportions)

# Crtanje grafika
ggplot(prop_df, aes(x = Axis, y = Proportion)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Proporcija objašnjene varijanse po osama", x = "Kanoničke ose", y = "Proporcija objašnjene varijanse") + scale_x_continuous(breaks = 1:9, labels = as.character(1:9)) 
```

``` {r}
# Prikaz težinskih koeficijenata za CCA1
coef(cca_model, choices = 1)
```

``` {r}
# Izvlačenje koeficijenata za prvu kanonsku osu i pravljenje data frame-a
cca_coef <- coef(cca_model, choices = "CCA1")  # Pobrinite se da izvlačite pravu osu
coef_df <- as.data.frame(cca_coef[, 1, drop = FALSE])  # Pretvara koeficijente u data frame
names(coef_df) <- "Coef"  # Imenuje kolonu
coef_df$Variables <- rownames(coef_df)  # Dodaje varijable kao kolonu

# Kreiranje bar grafikona koristeći ggplot2
p <- ggplot(coef_df, aes(x = Variables, y = Coef)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Koeficijenti za CCA1", x = "Ekološki Faktori", y = "Vrednost Koeficijenta") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotiranje labela za bolju čitljivost

# Prikaz grafikona
print(p)
```

### Biplot u kontekstu kanonske korelacione analize (CCA) vizualno prikazuje odnose između vrsta i ekoloških faktora u odnosu na kanonske ose.

``` {r}
library(plotly)
library(vegan)
library(stringr)

# Dobijanje skorova za lokacije i biplot skorova za ekološke faktore
site_scores <- scores(cca_model, display = "sites")
species_scores <- scores(cca_model, display = "species")
env_factors <- scores(cca_model, display = "bp", scaling = 3)  # Ekološki faktori, povećano skaliranje

# Kreiranje grupacija lokacija
grouped_site_labels <- str_extract(site_labels, "[A-Za-z]+")
group_colors <- c("ST" = "red", "PD" = "green", "LV" = "yellow", "KB" = "pink", "J" = "blueviolet", "V" = "cyan", "D" = "orange" )
colors <- group_colors[grouped_site_labels]
# colors <- RColorBrewer::brewer.pal(max(length(unique(grouped_site_labels))), "Set3")  # Generiše palete boja

# Kreiranje Plotly grafika
p <- plot_ly() %>%
  add_markers(
    x = site_scores[,1], y = site_scores[,2],
    color = as.factor(grouped_site_labels), colors = colors,
    text = grouped_site_labels,  # Tekst za prelazak mišem
    marker = list(size = 10),
    name = ~grouped_site_labels  # Dinamičko dodeljivanje imena za legendu
  )

# Dodavanje vektora za svaki ekološki faktor
for (i in 1:nrow(env_factors)) {
  factor_name <- rownames(env_factors)[i]
  p <- p %>%
    add_segments(
      x = 0, xend = env_factors[i, 1] * 2,  # Dužina linije
      y = 0, yend = env_factors[i, 2] * 2,
      line = list(color = 'blue', width = 2),
      name = factor_name, showlegend = FALSE
    ) %>%
    add_annotations(
      x = env_factors[i, 1] * 2, y = env_factors[i, 2] * 2,
      text = factor_name, showarrow = FALSE, xanchor = 'center', yanchor = 'bottom',
      showlegend = FALSE
    )
}

# Podešavanje layout-a
p <- p %>%
  layout(
    title = 'Grupisani CCA Biplot sa Ekološkim Faktorima',
    xaxis = list(title = 'CCA1'),
    yaxis = list(title = 'CCA2'),
    hovermode = 'closest',
    legend = list(title = "Grupacije Lokacija", x = 1.05, y = 1)
  )

# Prikazivanje grafika
p

```

``` {r}
# Kreiranje Plotly grafika za vrste sa labelama
p_species <- plot_ly() %>%
  add_markers(
    x = species_scores[,1], y = species_scores[,2],
    text = rownames(species_scores),  # Tekst za prelazak mišem (imena vrsta)
    marker = list(size = 10, color = 'red'),  # Boja tačaka za vrste
    name = "Species"  # Ime za legendu
  ) %>%
  add_text(
    x = species_scores[,1], y = species_scores[,2],
    text = rownames(species_scores),
    textposition = 'top right',
    showlegend = FALSE
  )

# Dodavanje vektora za svaki ekološki faktor (kao što je urađeno za lokacije)
for (i in 1:nrow(env_factors)) {
  factor_name <- rownames(env_factors)[i]
  p_species <- p_species %>%
    add_segments(
      x = 0, xend = env_factors[i, 1] * 2,  # Dužina linije
      y = 0, yend = env_factors[i, 2] * 2,
      line = list(color = 'blue', width = 2),
      name = factor_name, showlegend = FALSE
    ) %>%
    add_annotations(
      x = env_factors[i, 1] * 2, y = env_factors[i, 2] * 2,
      text = factor_name, showarrow = FALSE, xanchor = 'center', yanchor = 'bottom',
      showlegend = FALSE
    )
}

# Podešavanje layout-a za graf sa vrstama
p_species <- p_species %>%
  layout(
    title = 'Grupisani CCA Biplot sa Ekološkim Faktorima i Vrstama',
    xaxis = list(title = 'CCA1'),
    yaxis = list(title = 'CCA2'),
    hovermode = 'closest',
    legend = list(title = "Vrste", x = 1.05, y = 1)
  )

# Prikazivanje grafika
p_species
```

**Label Propagation**

je algoritam iz oblasti mašinskog učenja i teorije grafova koji se
koristi za rešavanje problema klasifikacije. Lp- algoritam je tip
delimično-nadglednih algoritama jer koristi i označene (labelisane) i
neoznačene podatke (nelabelisane) za treniranje modela.

Osnove algoritma:

-   Predstavljanje podataka kao graf - gde čvorovi predstavljaju
    podatke, a grane predstavljaju sličnosti među podacima

-   Inicijalizacija labela - Na početku čvorovi koji imaju poznate
    labele zadržavaju te labele, a oni koji nemaju su ili neoznačeni ili
    označeni sa nulom

-   Propagacija algoritma - Algoritam iterativno aržurira labele čvorova
    . Tokom svake iteracije svaki čvor aržurira svoj label na odnosu
    labela svojih suseda, često koristimo neki oblik glasanja tj. To
    znači da će čvor uzeti najučestaliji laben medju svojim susedima
    uzimajući u obzir težine grana

-   Algoritam se zaustavlja kada se labele stabilizuju tj. prestanu
    menjati izmedju iteracija ili nakon dostignutog unapred zadatog
    broja iteracija

``` {r}
#ucitavamo podatke
nodes<-read_excel("nodes.xlsx")
edges <- read_excel("edges.xlsx")

#pravimo graf
g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
set.seed(21)

# Set node attributes (assuming 'nodes' has columns 'name' and 'abudance')
V(g)$abudance <-nodes$Abudance

min_size <- 0.5
max_size <- 1
V(g)$size <- scales::rescale(V(g)$abudance, to = c(min_size, max_size))

# Perform label propagation
communities <- label.propagation.community(g,weights = E(g)$weight)

cat("Broj detektovanih zajednica", length(communities))
sizes(communities)

plot(g, vertex.color = communities$membership, vertex.size = V(g)$abudance, main = "Label Propagation")
```
