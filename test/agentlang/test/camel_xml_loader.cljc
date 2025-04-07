(ns agentlang.test.camel-xml-loader
  (:require
   [agentlang.lang.raw :as raw]
   [agentlang.lang.tools.camel-xml-loader.core :as camel]
   [agentlang.lang :refer [event entity dataflow]]
   [agentlang.component :as cn]
   #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
      :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])
   [clojure.test :refer :all]
   [agentlang.util :as u]))


(def test-xml-content
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<routeTemplates xmlns=\"http://camel.apache.org/schema/spring\">
    <routeTemplate id=\"CamelXmlTemplateResolver.Core/S3Upload\">
        <!-- FROM params -->
        <templateParameter name=\"LocalDirName\" defaultValue=\".\"/>
        <templateParameter name=\"LocalFileName\"/>
        <!-- TO params -->
        <templateParameter name=\"AwsAccessKey\"/>
        <templateParameter name=\"AwsSecretKey\"/>
        <templateParameter name=\"AwsRegion\"/>
        <templateParameter name=\"S3BucketName\"/>
        <templateParameter name=\"S3FileName\"/>
        <route>
            <from uri=\"file:{{LocalDirName}}?fileName={{LocalFileName}}&amp;noop=true\"/>
            <convertBodyTo type=\"byte[]\" />
            <setHeader name=\"CamelAwsS3ContentLength\">
                <simple>${in.header.CamelFileLength}</simple>
            </setHeader>
            <setHeader name=\"CamelAwsS3Key\">
                <simple>${in.header.CamelFileNameOnly}</simple>
            </setHeader>
            <to uri=\"aws2-s3://{{S3BucketName}}?accessKey={{AwsAccessKey}}&amp;secretKey={{AwsSecretKey}}&amp;region={{AwsRegion}}&amp;prefix={{S3FileName}}\"/>
        </route>
    </routeTemplate>
</routeTemplates>")


(deftest load-xml-content
  (testing "Load XML content and verify if it is in raw store"
    (camel/register-xml-templates test-xml-content)
    (is (= '(do (component :CamelXmlTemplateResolver.Core) (event :CamelXmlTemplateResolver.Core/S3Upload {:AwsAccessKey :String, :SleepMillis {:type :Int, :default 10000}, :LocalDirName {:type :String, :default "."}, :LocalFileName :String, :S3FileName :String, :BeanValues {:type :Map, :default {}}, :AwsRegion :String, :S3BucketName :String, :AwsSecretKey :String}))
           (raw/as-edn :CamelXmlTemplateResolver.Core))))
  (camel/register-xml-templates test-xml-content)
  (u/run-init-fns)
  (defcomponent :Test01
    (dataflow :CamelXmlTemplateResolver.Core/S3Upload
     :CamelXmlTemplateResolver.Core/S3Upload.S3BucketName)

    (dataflow
     :Test01/Create
     {:CamelXmlTemplateResolver.Core/S3Upload
      {:AwsAccessKey :Test01/Create.AwsAccessKey
       :LocalFileName :Test01/Create.LocalFileName
       :S3FileName :Test01/Create.S3FileName
       :AwsRegion :Test01/Create.AwsRegion
       :S3BucketName :Test01/Create.S3BucketName
       :AwsSecretKey :Test01/Create.AwsSecretKey}}))
  (let [invoke (tu/invoke {:Test01/Create
                           {:AwsAccessKey "something"
                            :LocalFileName "local_file"
                            :AwsSecretKey "secret"
                            :AwsRegion "us-west-2"
                            :S3BucketName "dummy-bucket"
                            :S3FileName "s3-file"}})]
    (is (= invoke "dummy-bucket"))))
