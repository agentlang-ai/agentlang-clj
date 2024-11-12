(component :School.Core)

(entity
 :School
 {:Name {:type :String :guid true}
  :RegistrationNumber {:type :String :unique true}
  :Address :String})

(attribute
 :Gender
 {:oneof ["M" "F"]})

(entity
 :Student
 {:EnrollmentNumber {:type :Int :id true}
  :Id :Identity
  :Name :String
  :Age :Int
  :Gender :Gender
  :EnrollmentDate :Date})

(entity
 :ParentInfo
 {:Address :String
  :ParentName :String
  :ParentEmail :Email
  :ParentPhoneNumber :String})

(relationship
 :ParentInfoForStudent
 {:meta {:between [:Student :ParentInfo]}})

(relationship
 :StudentOf
 {:meta {:contains [:School :Student]}})

(entity
 :Teacher
 {:StaffNumber {:type :Int :id true}
  :Id :Identity
  :Name :String
  :Age :Int
  :Gender :Gender
  :Designation :String
  :Qualification :String
  :JoiningDate :Date})

(entity
 :ContactInfo
 {:Address :String
  :Email :Email
  :PhoneNumber :String})

(relationship
 :TeacherContactInfo
 {:meta {:between [:Teacher :ContactInfo]}})

(relationship
 :StaffOf
 {:meta {:contains [:School :Teacher]}})

(entity
 :Class
 {:Id :Identity
  :Name :String
  :Grade :String
  :meta {:unique [:Name :Grade]}})

(entity
 :Subject
 {:Name {:type :String :guid true}
  :Texts {:listof :String}})

(relationship
 :ClassTeacher
 {:meta {:between [:Teacher :Class]}})

(relationship
 :TeachingAssignment
 {:meta {:between [:Teacher :Subject]}
  :AssignedOn :Now
  :Class {:ref :Class.Id}})

(relationship
 :Enrollment
 {:meta {:between [:Student :Class]}
  :EnrolledOn :Now})

(entity
 :Attendance
 {:Date :Now
  :Present :Boolean})

(relationship
 :ClassAttendance
 {:meta {:between [:Student :Attendance]}})
