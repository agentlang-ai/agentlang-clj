(component :Petstore.Core)

(dataflow :CreatePet {:SwaggerPetstoreOpenAPI30.Core/addPet {} :from :CreatePet})
;;; example:
;; {"Petstore.Core/CreatePet":
;;   {"id": 102,
;;    "category": {"id": 1, "name": "my-pets"},
;;    "name": "kittie",
;;    "photoUrls": ["https://mypets.com/imgs/kittie.jpg"],
;;    "tags": [{"id": 1, "name": "cats"}],
;;    "status": "available"}}

(dataflow :GetPetById {:SwaggerPetstoreOpenAPI30.Core/getPetById {:petId :GetPetById.id}})
;;; example:
;; {"Petstore.Core/GetPetById": {"id": 102}}
