module SimpleHTTP where

import Network.Curl

data Method = GET | POST | PUT | DELETE | HEAD | Other String deriving Show
type Url = String
data Request = Request { url :: Url
                       , method :: Method
                       , options :: [Option] }
data Response = Response { body :: String
                         , code :: Int }
data Option = Auth String String
            | PostFields [String]
instance Show Option where
    show (Auth u p) = u ++ ":" ++ p

parseOption :: Option -> [CurlOption]
parseOption (Auth u p) = [CurlHttpAuth [HttpAuthBasic], CurlUserPwd $ show (Auth u p)]
parseOption (PostFields fs) = [CurlPostFields fs]

parseMethod :: Method -> [CurlOption]
parseMethod (Other a) = [CurlCustomRequest a]
parseMethod GET = method_GET
parseMethod POST = method_POST
parseMethod HEAD = method_HEAD
parseMethod method = [CurlCustomRequest $ show method]

doRequest :: Request -> IO Response
doRequest (Request url method options) = do
    curl <- initialize
    response <- do_curl_ curl url (foldr (++) (parseMethod method) (map parseOption options)) :: IO CurlResponse
    return $ Response (respBody response) (respStatus response) :: IO Response

get_url :: Url -> IO Response
get_url u = doRequest (Request u GET [])
