module Util where

import ServerAPI ( SPParams_(..)
                 , getApiEmailByEmail
                 )

getSubscriptionStatus :: ExceptT AjaxError
			   ( ReaderT ( SPSettings_ SPParams_ )
			     ( Aff ( ajax :: AJAX | eff ) ) )
                               EmailProperties
getSubscriptionStatus = getApiEmailByEmail
