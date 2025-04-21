(component :Petstore.Core)

(dataflow :CreatePet {:SwaggerPetstoreOpenAPI30/addPet {} :from :CreatePet})
;;; example:
;; {"Petstore.Core/CreatePet":
;;  {"Parameters":
;;   {"id": 102,
;;    "category": {"id": 1, "name": "my-pets"},
;;    "name": "kittie",
;;    "photoUrls": ["https://mypets.com/imgs/kittie.jpg"],
;;    "tags": [{"id": 1, "name": "cats"}],
;;    "status": "available"}}}

(dataflow :GetPetById {:SwaggerPetstoreOpenAPI30/getPetById {:petId :GetPetById.id}})
;;; example:
;; {"Petstore.Core/GetPetById": {"id": 102}}
