;;; guilecraft --- learning the world using Guile.         -*- coding: utf-8 -*-

(define-module (glean store birds discipline)
  #:use-module (glean library core-templates)
  #:export (birds-module))

(define swans
  (set 'swans
       #:contents
       (list
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/0/01/Mute_swan_%28cygnus_olor%29.JPG/240px-Mute_swan_%28cygnus_olor%29.JPG")))
                 (s "swan"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/4/4a/Cygnus_bewickii_01.jpg/240px-Cygnus_bewickii_01.jpg")))
                 (s "swan"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/5/54/Singschwan.jpg/153px-Singschwan.jpg")))
                 (s "swan")))))

(define geese
  (set 'geese
       #:contents
       (list
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/1/15/Bean.goose.600pix.jpg/155px-Bean.goose.600pix.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/f/fe/Anser_brachyrhynchus.jpg/197px-Anser_brachyrhynchus.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/3/35/White-fronted.goose.750pix.jpg/204px-White-fronted.goose.750pix.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/9/97/Anser_erythropus.jpg/181px-Anser_erythropus.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Greylag_goose_swimming_%28anser_anser%29.jpg/240px-Greylag_goose_swimming_%28anser_anser%29.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/7/7a/Chen_caerulescens_32398.JPG/213px-Chen_caerulescens_32398.JPG")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/6/65/Branta_canadensis_%285%29.JPG/155px-Branta_canadensis_%285%29.JPG")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Branta_leucopsis_-standing_in_a_field-8.jpg/114px-Branta_leucopsis_-standing_in_a_field-8.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/7/7d/Brent_Goose.jpg/240px-Brent_Goose.jpg")))
                 (s "goose"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/7/70/Red-breasted_goose_arp.jpg/224px-Red-breasted_goose_arp.jpg")))
                 (s "goose"))
(problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/6/6a/Alopochen-aegyptiacus.jpg/194px-Alopochen-aegyptiacus.jpg")))
                 (s "goose")))))

(define ducks
  (set 'ducks
       #:contents
       (list
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/9/92/Ruddy_shelduck_arp.jpg/224px-Ruddy_shelduck_arp.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/e/ea/Common_Shelduck_%28Tadorna_tadorna%29_at_Sylvan_Heights.jpg/213px-Common_Shelduck_%28Tadorna_tadorna%29_at_Sylvan_Heights.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/8/85/Aix_galericulata_-_Z%C3%BCrich_-_Hafen_Riesbach_2011-01-14_15-58-32.JPG/241px-Aix_galericulata_-_Z%C3%BCrich_-_Hafen_Riesbach_2011-01-14_15-58-32.JPG")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/2/25/Anas_penelope_kuribo_cropped.jpg/238px-Anas_penelope_kuribo_cropped.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/b/b5/Anas_americana_-_drake.jpg/213px-Anas_americana_-_drake.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/d/d1/Anas-strepera-001.jpg/240px-Anas-strepera-001.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/2/21/Anas.formosa.4.jpg/193px-Anas.formosa.4.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/f/f0/Common_Teal_%28Anas_crecca%29_near_Hodal%2C_Haryana_W_IMG_6512.jpg/211px-Common_Teal_%28Anas_crecca%29_near_Hodal%2C_Haryana_W_IMG_6512.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/a/a4/Anas_carolinensis_%28Green-winged_Teal%29_male.jpg/240px-Anas_carolinensis_%28Green-winged_Teal%29_male.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/b/bf/Anas_platyrhynchos_male_female_quadrat.jpg/160px-Anas_platyrhynchos_male_female_quadrat.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/a/a9/American_Black_Duck_male_RWD1.jpg/213px-American_Black_Duck_male_RWD1.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Northern_Pintails_%28Male_%26_Female%29_I_IMG_0911.jpg/274px-Northern_Pintails_%28Male_%26_Female%29_I_IMG_0911.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/a/a9/Garganey_%28Anas_querquedula%29_RWD3.jpg/214px-Garganey_%28Anas_querquedula%29_RWD3.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/6/65/Blue-Winged_Teal.jpg/240px-Blue-Winged_Teal.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/e/e9/Northern_Shoveler_Anas_clypeata.jpg/240px-Northern_Shoveler_Anas_clypeata.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/5/50/Netta_rufina_%28Red-crested_Pochard%29_Male%2C_London_Wetland_Centre_-_Diliff.jpg/240px-Netta_rufina_%28Red-crested_Pochard%29_Male%2C_London_Wetland_Centre_-_Diliff.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/f/ff/Canvasback.arp.750pix.jpg/224px-Canvasback.arp.750pix.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/5/5c/Aythya_ferina_Sandwell_2.jpg/212px-Aythya_ferina_Sandwell_2.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/0/0b/Redhead_duck_%28Aythya_americana%2C_male%29.jpg/210px-Redhead_duck_%28Aythya_americana%2C_male%29.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Aythya-collaris-001.jpg/240px-Aythya-collaris-001.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/2/28/Ferruginous_Duck_RWD.jpg/213px-Ferruginous_Duck_RWD.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/4/41/Aythya-fuligula_Tufted-Duck.jpg/236px-Aythya-fuligula_Tufted-Duck.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/f/fb/Greater-scaup-male2.jpg/247px-Greater-scaup-male2.jpg")))
                 (s "duck"))
        (problem (q "Using the image below, what type of bird is this?"
                    (media #:images `("//upload.wikimedia.org/wikipedia/commons/thumb/1/12/Veilchenente_Aythya_affinis_0505282.jpg/213px-Veilchenente_Aythya_affinis_0505282.jpg")))
                 (s "duck")))))

(define birds-module
  (module
    'birds
    #:name "Bird Recognition"
    #:version "0.1"
    #:keywords '("ornithology" "ecology" "environment" "biology")
    #:synopsis "Learn about the tell-tale signs of different bird species."
    #:description ""
    #:creator "Alex Sassmannshausen"
    #:attribution (list (media #:urls '("http://www.wikipedia.org")))
    #:contents `(,ducks ,geese ,swans)
    #:resources (list (media #:urls '("http://www.wikipedia.org")))))
