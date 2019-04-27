library("ggplot2")


df = read.csv("~/solar_system/processed_data.csv")

planets = df[grepl("planet",df$type),]
planets = planets[!(grepl("dwarf",planets$type)),]

planets$mass_kg = as.numeric(as.character(planets$mass_kg))
planets$volume_km = as.numeric(planets$volume_km)



ggplot(planets, aes(x= mass_kg, y= volume_km,
                     label=body,colour = factor(type)))+
  geom_point() +
  geom_text(aes(label=body),hjust=0,vjust=0,
            check_overlap = TRUE)


ggplot(planets, aes(x= radius_r, y= gravity_g,
                    label=body,colour = factor(type)))+
  geom_point() +
  geom_text(aes(label=body),hjust=0,vjust=0,
            check_overlap = TRUE)

