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

parse_option :: Option -> [CurlOption]
parse_option (Auth u p) = [CurlHttpAuth [HttpAuthBasic], CurlUserPwd $ show (Auth u p)]
parse_option (PostFields fs) = [CurlPostFields fs]

parse_method :: Method -> [CurlOption]
parse_method (Other a) = [CurlCustomRequest a]
parse_method GET = method_GET
parse_method POST = method_POST
parse_method HEAD = method_HEAD
parse_method method = [CurlCustomRequest $ show method]

do_request :: Request -> IO Response
do_request (Request url method options) = do
    curl <- initialize
    response <- do_curl_ curl url (foldl (++) (parse_method method) (map parse_option options)) :: IO CurlResponse
    return $ Response (respBody response) (respStatus response) :: IO Response

get_url :: Url -> IO Response
get_url u = do_request (Request u GET [])
