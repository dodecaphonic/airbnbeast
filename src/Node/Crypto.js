import crypto from 'crypto';

export const randomBytesImpl = function(length) {
  return function() {
    try {
      const bytes = crypto.randomBytes(length);
      return bytes.toString('hex');
    } catch (error) {
      throw error;
    }
  };
};

export const hmacImpl = function(secret, data) {
  const hmac = crypto.createHmac('sha256', secret);
  hmac.update(data);
  return hmac.digest('hex');
};